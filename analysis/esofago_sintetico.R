# =============================================================================
# CÂNCER DE ESÔFAGO, PRÉ x PÓS PROTOCOLO | GERADOR DA COORTE SINTÉTICA
# -----------------------------------------------------------------------------
# Le....: base real do TCC (prontuários) — fica apenas na máquina local
# Grava.: data/coorte_esofago_sintetica.csv  (pacientes fictícios)
#         analysis/esofago_resultados_reais.rds  (só coeficientes e testes)
# Ideia.: as covariáveis seguem as proporções reais dentro de cada grupo de
#         protocolo, e o tempo até o óbito vem do Cox ajustado real com a linha
#         de base de Breslow; nenhuma linha real é copiada para a base sintética
# =============================================================================

library(data.table)
library(readxl)
library(lubridate)
library(survival)

PASTA_REAL <- Sys.getenv("PASTA_ESOFAGO")

# -----------------------------------------------------------------------------
# 1) BASE REAL — mesmo tratamento de dados do script original (script_joana_v4.R)
# -----------------------------------------------------------------------------
codigo <- readLines(file.path(PASTA_REAL, "script_joana_v4.R"), encoding = "UTF-8")[1:87]
codigo <- sub("path = \"", paste0("path = \"", gsub("\\\\", "/", PASTA_REAL), "/"), codigo, fixed = TRUE)
invisible(capture.output(suppressMessages(suppressWarnings(eval(parse(text = codigo))))))
real <- copy(dataset2)

DATA_CORTE <- max(real$data_obito, na.rm = TRUE)   # fim da coleta: último óbito registrado
real[, tempo := fifelse(obito_geral == 1, as.numeric(data_obito - consulta_1), as.numeric(DATA_CORTE - consulta_1)) / 30.44]
real[, ecog_c := fifelse(is.na(ecog), "Not recorded", ecog)]
real[, protocolo := factor(protocolo, c("Pré Protocolo", "Pós Protocolo"))]
real <- real[!is.na(tempo) & !is.na(estadiamento) & !is.na(fumante) & !is.na(etilismo)]

# -----------------------------------------------------------------------------
# 2) RESULTADOS REAIS AGREGADOS — para comparar com a análise sintética
# -----------------------------------------------------------------------------
formula_conf <- Surv(tempo, obito_geral) ~ protocolo + estadiamento + ecog_c + faixa_etaria + sexo + fumante + etilismo
lr_correto  <- survdiff(Surv(tempo, obito_geral) ~ protocolo, real)
cox_bruto   <- coxph(Surv(tempo, obito_geral) ~ protocolo, real)
cox_ajust   <- coxph(formula_conf, real)

resumo_cox <- function(m) {
  s <- summary(m)$conf.int
  data.table(termo = rownames(s), HR = s[, 1], ic_inf = s[, 3], ic_sup = s[, 4], p = summary(m)$coefficients[, 5])
}
saveRDS(list(
  p_correto  = pchisq(lr_correto$chisq, 1, lower.tail = FALSE),  n_correto  = sum(lr_correto$n),
  cox_bruto  = resumo_cox(cox_bruto), cox_ajust = resumo_cox(cox_ajust),
  schoenfeld_global = cox.zph(cox_ajust)$table["GLOBAL", "p"]
), "analysis/esofago_resultados_reais.rds")

# -----------------------------------------------------------------------------
# 3) COORTE SINTÉTICA
#    covariáveis: sorteadas com as proporções reais de cada grupo de protocolo
#    tempo: Cox ajustado real + linha de base de Breslow, censura administrativa no corte
# -----------------------------------------------------------------------------
weibull <- survreg(formula_conf, real, dist = "weibull")   # usado só para guardar os níveis dos fatores

# linha de base: risco acumulado de Breslow do Cox ajustado real (x = 0),
# o que preserva a forma real do risco ao longo do tempo
H0 <- basehaz(cox_ajust, centered = FALSE)

sorteia <- function(x, n) {
  p <- prop.table(table(x))
  sample(names(p), n, replace = TRUE, prob = p)
}

N_POR_GRUPO <- c("Pré Protocolo" = 99, "Pós Protocolo" = 85)
JANELA <- list("Pré Protocolo" = as.Date(c("2016-01-01", "2018-07-31")),
               "Pós Protocolo" = as.Date(c("2018-08-01", "2020-12-31")))
VARIAVEIS <- c("estadiamento", "ecog_c", "faixa_etaria", "sexo", "fumante", "etilismo", "tempo_consulta_1_trat", "tratamento")

simula_coorte <- function(semente) {
  set.seed(semente)
  coorte <- rbindlist(lapply(names(N_POR_GRUPO), function(g) {
    n <- N_POR_GRUPO[[g]]
    base_g <- real[protocolo == g]
    d <- as.data.table(setNames(lapply(VARIAVEIS, function(v) sorteia(base_g[[v]], n)), VARIAVEIS))
    d[, protocolo := g]
    d[, consulta_1 := JANELA[[g]][1] + sample(0:as.integer(diff(JANELA[[g]])), n, replace = TRUE)]
    d
  }))
  coorte[, `:=`(fumante = as.integer(fumante), etilismo = as.integer(etilismo))]
  coorte[, protocolo := factor(protocolo, levels(real$protocolo))]

  # tempo de vida: Cox ajustado real com a linha de base de Breslow
  #   S(t | x) = exp(-H0(t) * exp(x'beta))  =>  T = H0^-1(-log U / exp(x'beta))
  #   alvos além do último tempo observado viram Inf (ficam censurados no corte)
  X <- model.matrix(delete.response(terms(formula_conf)), coorte, xlev = weibull$xlevels)[, names(coef(cox_ajust))]
  risco <- as.numeric(exp(X %*% coef(cox_ajust)))
  alvo <- -log(runif(nrow(coorte))) / risco
  tempo_obito <- approx(H0$hazard, H0$time, xout = alvo, ties = "ordered", rule = 1)$y
  tempo_obito[alvo < min(H0$hazard)] <- min(H0$time) * alvo[alvo < min(H0$hazard)] / min(H0$hazard)
  tempo_obito[is.na(tempo_obito)] <- Inf
  tempo_censura <- as.numeric(DATA_CORTE - coorte$consulta_1) / 30.44

  coorte[, obito := as.integer(tempo_obito <= tempo_censura)]
  coorte[, tempo_meses := round(pmin(tempo_obito, tempo_censura), 2)]
  coorte[, data_obito := fifelse(obito == 1L, consulta_1 + round(tempo_meses * 30.44), as.Date(NA))]
  coorte[, paciente := sprintf("P%03d", .I)]
  coorte
}

# -----------------------------------------------------------------------------
# 4) QUAL COORTE PUBLICAR?
#    com n = 184 uma única coorte varia muito; 300 coortes são simuladas, a
#    distribuição do HR do protocolo é guardada (vai para a página) e é
#    publicada a coorte mais típica: HR ajustado mais próximo da mediana
# -----------------------------------------------------------------------------
hr_protocolo <- function(d) {
  d <- copy(d)[, protocolo := factor(protocolo, levels(real$protocolo))]
  exp(coef(coxph(update(formula_conf, Surv(tempo_meses, obito) ~ .), d))[["protocoloPós Protocolo"]])
}
simulacoes <- rbindlist(lapply(1:300, function(s) {
  d <- simula_coorte(s)
  lr <- survdiff(Surv(tempo_meses, obito) ~ protocolo, d)
  data.table(semente = s, hr = hr_protocolo(d), p_logrank = pchisq(lr$chisq, 1, lower.tail = FALSE))
}))
semente_tipica <- simulacoes[which.min(abs(log(hr) - log(median(hr)))), semente]
saveRDS(simulacoes[, .(hr, p_logrank)], "analysis/esofago_simulacoes.rds")

coorte <- simula_coorte(semente_tipica)
setcolorder(coorte, c("paciente", "protocolo", "consulta_1", "data_obito", "obito", "tempo_meses"))
fwrite(coorte, "data/coorte_esofago_sintetica.csv")

cat("Synthetic cohort:", nrow(coorte), "patients,", sum(coorte$obito), "deaths\n")
print(coorte[, .(n = .N, obitos = sum(obito)), by = protocolo])

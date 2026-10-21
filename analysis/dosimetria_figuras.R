# =============================================================================
# DOSIMETRIA DA PENA (TJ-SP, 2022) | FIGURAS E TABELAS DA PÁGINA DO PORTFÓLIO
# -----------------------------------------------------------------------------
# Le....: parquets da pasta de replicação do OSF (uma base por crime x fase)
# Grava.: images/project04/*.png e analysis/project04_tabelas.rds
# Uso...: só agregados saem deste script; nenhuma linha individual é exportada
# Dados.: públicos no OSF (https://osf.io/vr4zf e https://osf.io/23ytq)
# =============================================================================

library(data.table)
library(arrow)
library(ggplot2)

PASTA_DADOS <- Sys.getenv("PASTA_DOSIMETRIA")   # pasta com os parquets do OSF
PASTA_IMG   <- "images/project04"
dir.create(PASTA_IMG, showWarnings = FALSE, recursive = TRUE)

CRIMES <- c(FURTO_SIMPLES = "Simple theft", FURTO_QUALIFICADO = "Aggravated theft",
            ROUBO = "Robbery", TRAFICO = "Drug trafficking")
FASES  <- c("1" = "Phase 1\nBase sentence", "2" = "Phase 2\nProvisional", "3" = "Phase 3\nDefinitive")

ler <- function(crime, fase) as.data.table(read_parquet(file.path(PASTA_DADOS, sprintf("%s_FASE_%s.parquet", crime, fase))))

CORES <- setNames(c("#7c3aed", "#db2777", "#0891b2", "#65a30d"), CRIMES)   # mesma cor por crime em todos os gráficos

tema <- theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
        legend.position = "top", strip.text = element_text(face = "bold"))

# -----------------------------------------------------------------------------
# 1) PLANO AMOSTRAL — tamanho mínimo para população finita
#    n = N z² p(1-p) / (e² (N-1) + z² p(1-p)),  z = 1.96, e = 5%, p = 0.5
# -----------------------------------------------------------------------------
amostra <- data.table(
  crime     = CRIMES,
  populacao = c(487, 695, 1695, 26919),   # condenações em 2022 (TJ-SP)
  coletada  = c(266, 288, 330, 377)
)
amostra[, n_minimo := ceiling(populacao * 1.96^2 * 0.25 / (0.05^2 * (populacao - 1) + 1.96^2 * 0.25))]
amostra[, n_final := sapply(names(CRIMES), function(cr) nrow(ler(cr, "FINAL")))]

# -----------------------------------------------------------------------------
# 2) PENA AO LONGO DAS FASES — mediana e coeficiente de variação
# -----------------------------------------------------------------------------
penas <- rbindlist(lapply(names(CRIMES), function(cr) rbindlist(lapply(c("1", "2", "3", "FINAL"), function(f)
  ler(cr, f)[, .(crime = CRIMES[[cr]], fase = f, penaemdias)]))))

evolucao <- penas[, .(mediana = median(penaemdias, na.rm = TRUE),
                      cv = sd(penaemdias, na.rm = TRUE) / mean(penaemdias, na.rm = TRUE)), by = .(crime, fase)]
evolucao[, fase := factor(fase, c("1", "2", "3", "FINAL"), c("Phase 1", "Phase 2", "Phase 3", "Final"))]
evolucao[, crime := factor(crime, CRIMES)]

g <- ggplot(evolucao, aes(fase, cv, colour = crime, group = crime)) +
  geom_line(linewidth = 1.3) + geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = CORES) +
  labs(title = "Dispersion of the sentence grows phase after phase",
       x = NULL, y = "Coefficient of variation", colour = NULL) + tema
ggsave(file.path(PASTA_IMG, "cv_por_fase.png"), g, width = 10, height = 5.5, dpi = 150, bg = "white")

# -----------------------------------------------------------------------------
# 3) IMPACTO DE CADA FASE — variação absoluta média da pena (dias)
# -----------------------------------------------------------------------------
impacto <- rbindlist(lapply(names(CRIMES), function(cr) {
  p <- lapply(c("1", "2", "3"), function(f) ler(cr, f)[, .(processo, penaemdias)])
  base <- merge(merge(p[[1]], p[[2]], by = "processo", suffixes = c("_1", "_2")), p[[3]], by = "processo")
  data.table(crime = CRIMES[[cr]], fase = c("2", "3"),
             media_abs = c(mean(abs(base$penaemdias_2 - base$penaemdias_1), na.rm = TRUE),
                           mean(abs(base$penaemdias - base$penaemdias_2), na.rm = TRUE)),
             prop_mudou = c(mean(base$penaemdias_2 != base$penaemdias_1, na.rm = TRUE),
                            mean(base$penaemdias != base$penaemdias_2, na.rm = TRUE)))
}))

# -----------------------------------------------------------------------------
# 4) USO DE FRAÇÕES — qual fração o juiz declarou em cada fase
#    o texto livre é normalizado para a primeira fração citada
# -----------------------------------------------------------------------------
normaliza_fracao <- function(x) {
  fr <- regmatches(x, regexpr("[0-9]+/[0-9]+", x))
  out <- rep(NA_character_, length(x))
  out[grepl("[0-9]+/[0-9]+", x)] <- fr
  out[is.na(out) & !is.na(x)] <- "Not specified"
  out
}
colunas_fracao <- list("1" = "pena_base_majorada",
                       "2" = c("pena_provisoria_majorada", "pena_base_majorada", "pena_majorada"),
                       "3" = c("pena_majorada", "pena_definitiva_majorada"))

fracoes <- rbindlist(lapply(names(CRIMES), function(cr) rbindlist(lapply(c("1", "2", "3"), function(f) {
  d <- ler(cr, f)
  col <- intersect(colunas_fracao[[f]], names(d))[1]
  if (is.na(col)) return(NULL)
  data.table(crime = CRIMES[[cr]], fase = f, fracao = normaliza_fracao(d[[col]]))
}))))
fracoes <- fracoes[!is.na(fracao)]   # NA = a fase não aumentou a pena
fracoes[, fracao := fifelse(fracao %in% c("1/6", "1/3", "1/4", "1/5", "2/3", "1/2", "1/8", "3/8", "Not specified"), fracao, "Other")]
uso_frac <- fracoes[, .N, by = .(crime, fase, fracao)][, prop := N / sum(N), by = .(crime, fase)]
uso_frac[, fracao := factor(fracao, c("1/8", "1/6", "1/5", "1/4", "1/3", "3/8", "1/2", "2/3", "Other", "Not specified"))]
uso_frac[, fase := factor(fase, names(FASES), FASES)]
uso_frac[, crime := factor(crime, rev(CRIMES))]

g <- ggplot(uso_frac, aes(fracao, crime, fill = prop)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = fifelse(prop >= 0.005, scales::percent(prop, accuracy = 1), "")), size = 3.6) +
  facet_wrap(~fase, ncol = 1) +
  scale_fill_gradient(low = "#f1f5f9", high = "#1d4ed8", labels = scales::percent) +
  labs(title = "Which fraction did the judge declare when increasing the sentence?",
       x = NULL, y = NULL, fill = "Share of\nincreases") + tema +
  theme(panel.grid = element_blank(), legend.position = "right")
ggsave(file.path(PASTA_IMG, "uso_fracoes.png"), g, width = 11, height = 8.5, dpi = 150, bg = "white")

# -----------------------------------------------------------------------------
# 5) MODELOS — os 12 modelos do script original, um por crime x fase
#    o script é executado tal como foi entregue; cada lm() é capturado
# -----------------------------------------------------------------------------
codigo <- readLines(file.path(PASTA_DADOS, "modelos_regressao_v2.R"), encoding = "UTF-8")
codigo <- gsub("read_parquet\\(\"", "read_parquet(\"__PASTA__/", codigo)
codigo <- gsub("__PASTA__", gsub("\\\\", "/", PASTA_DADOS), codigo, fixed = TRUE)
codigo <- gsub("^fit <- lm\\(", "fit <- MODELOS[[length(MODELOS) + 1]] <- lm(", codigo)

MODELOS <- list()
invisible(capture.output(eval(parse(text = codigo))))
stopifnot(length(MODELOS) == 12)

rotulos <- expand.grid(fase = c("1", "2", "3"), crime = CRIMES, stringsAsFactors = FALSE)
coeficientes <- rbindlist(lapply(seq_along(MODELOS), function(i) {
  m <- MODELOS[[i]]; ic <- confint(m)
  data.table(crime = rotulos$crime[i], fase = rotulos$fase[i], termo = names(coef(m)),
             estimativa = coef(m), ic_inf = ic[, 1], ic_sup = ic[, 2],
             p = summary(m)$coefficients[, 4], r2_aj = summary(m)$adj.r.squared, n = nobs(m))
}))[termo != "(Intercept)"]

nomes_termos <- c(antecedentes = "Criminal record", personalidade_agente = "Personality of the agent",
  circunstancia_crime = "Circumstances of the crime", consequencia_crime = "Consequences of the crime",
  culpabilidade = "Culpability", circunstancia_preponderantes = "Nature/quantity of the drug",
  usou_frac = "Judge declared a fraction", reincidencia = "Recidivism", confissao = "Confession",
  incendio = "Public calamity (e.g. COVID-19)", menoridade = "Defendant under 21 or over 70", contra_minoria = "Victim child/elderly",
  crime_tentado = "Attempted crime", crime_noturno = "Committed at night", reu_primario = "First offender",
  arrependimento = "Repentance", concurso_2_mais_pessoas = "Two or more agents",
  crime_cadeia = "Inside a prison", trafico_federal = "Interstate trafficking",
  crime_adolescente = "Involving a teenager", trafico_privilegiado = "Privileged trafficking",
  restricao_liberdade_vitima = "Victim held captive", violencia_arma_branca = "Bladed weapon",
  violencia_arma_fogo = "Firearm")
coeficientes[, variavel := sub("Sim$", "", termo)]
coeficientes[, rotulo := fifelse(variavel %in% names(nomes_termos), nomes_termos[variavel], variavel)]
coeficientes[, crime := factor(crime, CRIMES)]

ajuste <- unique(coeficientes[, .(crime, fase, r2_aj, n)])

g <- ggplot(coeficientes[fase %in% c("2", "3")], aes(estimativa, reorder(rotulo, estimativa), colour = crime)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = ic_inf, xmax = ic_sup), position = position_dodge(width = 0.6), size = 0.4) +
  facet_wrap(~ factor(fase, c("2", "3"), c("Phase 2: aggravating / mitigating", "Phase 3: causes of increase / decrease")),
             scales = "free", ncol = 1) +
  labs(title = "How many days each legal factor adds or removes",
       subtitle = "Outcome: change from the previous phase, in days (95% CI)",
       x = "Effect on the sentence (days)", y = NULL, colour = NULL) +
  scale_colour_manual(values = CORES) + tema
ggsave(file.path(PASTA_IMG, "coeficientes.png"), g, width = 11, height = 10, dpi = 150, bg = "white")

# -----------------------------------------------------------------------------
# 6) CONFISSÃO — em quantos casos ela NÃO reduziu a pena (Súmula 231 do STJ)
#    só casos com confissão e sem nenhuma agravante na fase 2: se a pena
#    provisória ficou igual à pena-base, a atenuante não teve efeito
# -----------------------------------------------------------------------------
confissao <- rbindlist(lapply(names(CRIMES), function(cr) {
  d1 <- ler(cr, "1")[, .(processo, pena_base = penaemdias)]
  d2 <- ler(cr, "2")
  agravantes <- setdiff(names(d2)[sapply(d2, function(x) all(na.omit(x) %in% c("Sim", "Não", "sim", "não")))],
                        c("confissao", "circunstancia_agravante", "circunstancia_agravantes", "circ_favoraveis",
                          "pena_provisoria_igual_base", "pena_igual_base"))
  d2 <- d2[tolower(confissao) == "sim"]
  d2 <- d2[d2[, Reduce(`&`, lapply(.SD, function(x) is.na(x) | tolower(x) == "não")), .SDcols = agravantes]]
  d2 <- merge(d2[, .(processo, pena_provisoria = penaemdias)], d1, by = "processo")
  data.table(crime = CRIMES[[cr]], n = nrow(d2),
             sem_reducao = mean(d2$pena_provisoria == d2$pena_base),
             reducao_media = d2[pena_provisoria < pena_base, mean(pena_base - pena_provisoria)])
}))

saveRDS(list(amostra = amostra, evolucao = evolucao, impacto = impacto, ajuste = ajuste,
             coeficientes = coeficientes, confissao = confissao),
        "analysis/project04_tabelas.rds")

print(amostra); print(evolucao); print(impacto); print(ajuste); print(confissao)
print(coeficientes[, .(crime, fase, rotulo, estimativa = round(estimativa), p = signif(p, 2))])

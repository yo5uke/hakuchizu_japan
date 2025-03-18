# GISデータの読み込み（rdsファイルの有無で分岐）
load_gis_data <- function(geojson_path = "data/japan_municipalities.shp",
                          rds_path = "data/japan_municipalities.rds",
                          target_crs = 6668) {
  if (file.exists(rds_path)) {
    gis <- read_rds(rds_path)
  } else {
    gis <- read_sf(geojson_path)|> 
      st_transform(crs = st_crs(target_crs))
    
    write_rds(gis, rds_path)
  }
  return(gis)
}

gis <- load_gis_data() |> 
  rename(
    pref = N03_001,
    muni = N03_004
  )

# 都道府県名を抽出
prefs <- gis |> 
  pull(pref) |> 
  unique()

# 選択された都道府県に対応する市町村の候補を生成
generate_muni_choices <- function(gis_data, selected_pref) {
  gis_data |> 
    filter(pref == selected_pref) |> 
    pull(muni) |> 
    unique()
}

# 都道府県ごとのデータをキャッシュ
load_prefecture_data <- function(gis_data, selected_pref, rds_dir = "data/prefecture/") {
  if (!dir.exists(rds_dir)) {
    dir.create(rds_dir, recursive = TRUE)
  }
  rds_path <- file.path(rds_dir, paste0(selected_pref, ".rds"))
  if (file.exists(rds_path)) {
    pref_data <- readRDS(rds_path)
  } else {
    pref_data <- gis_data |> filter(pref == selected_pref)
    saveRDS(pref_data, rds_path)
  }
  return(pref_data)
}

# 地図を描画
# ・focus_muniがTRUEかつselected_regionが指定されていれば対象市町村のみ描画
# ・それ以外は都道府県全体を描画し該当する市町村をハイライト
render_map <- function(pref_region, selected_region = NULL, focus_muni = FALSE, fill_color = "#CC6666") {
  if (isTRUE(focus_muni) && !is.null(selected_region)) {
    ggplot() +
      geom_sf(data = selected_region, fill = fill_color, color = "black") +
      theme_void()
  } else {
    p <- ggplot() +
      geom_sf(data = pref_region, fill = "white", color = "black") +
      theme_void()
    if (!is.null(selected_region)) {
      p <- p + geom_sf(data = selected_region, fill = fill_color, color = "black")
    }
    p
  }
}
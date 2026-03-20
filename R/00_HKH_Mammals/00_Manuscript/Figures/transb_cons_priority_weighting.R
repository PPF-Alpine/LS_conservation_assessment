---------------------------------------------#
  # get border segments, countries and cons priority
  #---------------------------------------------#
  
border_segments_cons_prio <- sf::st_read(paste0(data_storage_path, "Output/transboundary/border_segments_consprio.gpkg"))
  
transb_species <- sf::st_read(paste0(data_storage_path, "Output/transboundary/transb_glob_threat_new.shp"))
  
quantile(c(border_segments_cons_prio$npixA,
           border_segments_cons_prio$npixB),
         probs = 0.9, na.rm = TRUE)


filtered_df <- border_segments_cons_prio |>
  filter(pmin(npixA, npixB) > 6000)


# ---------------------------------------------#
  # with absolute
  #---------------------------------------------#



plot_df <- filtered_df |>
  st_drop_geometry() |>
  mutate(
    # total weighted priority (protected + unprotected)
    I_A = 1 * (c1A + c4A) +
      2 * (c2A + c5A) +
      3 * (c3A + c6A),
    
    I_B = 1 * (c1B + c4B) +
      2 * (c2B + c5B) +
      3 * (c3B + c6B),
    
    diff_I = abs(I_A - I_B),
    
    # weighted unprotected priority
    G_A = 1 * c4A + 2 * c5A + 3 * c6A,
    G_B = 1 * c4B + 2 * c5B + 3 * c6B,
    
    diff_G = abs(G_A - G_B),
    
    # optional total protected priority
    P_A = 1 * c1A + 2 * c2A + 3 * c3A,
    P_B = 1 * c1B + 2 * c2B + 3 * c3B,
    
    diff_P = abs(P_A - P_B)
  ) |>
  left_join(
    transb_species |>
      st_drop_geometry() |>
      select(seg_id, n_shard),
    by = "seg_id"
  )










######## interactive

plot_df <- plot_df |>
  mutate(
    hover_txt = paste0(
      "<b>", pair_id, "</b>",
      "<br>Segment: ", seg_id,
      "<br>Shared threatened species: ", n_shard,
      "<br>Δ weighted priority (I): ", round(diff_I, 3),
      "<br>Δ unprotected priority (G): ", round(diff_G, 3),
      "<br><br><b>", ctryA, "</b>",
      "<br>Protected: ", round(p1A + p2A + p3A, 3),
      "<br>Unprotected: ", round(p4A + p5A + p6A, 3),
      "<br>p1=", round(p1A, 3), "  p2=", round(p2A, 3), "  p3=", round(p3A, 3),
      "<br>p4=", round(p4A, 3), "  p5=", round(p5A, 3), "  p6=", round(p6A, 3),
      "<br><br><b>", ctryB, "</b>",
      "<br>Protected: ", round(p1B + p2B + p3B, 3),
      "<br>Unprotected: ", round(p4B + p5B + p6B, 3),
      "<br>p1=", round(p1B, 3), "  p2=", round(p2B, 3), "  p3=", round(p3B, 3),
      "<br>p4=", round(p4B, 3), "  p5=", round(p5B, 3), "  p6=", round(p6B, 3)
    )
  )



p_int <- plot_ly(
  data = plot_df,
  x = ~diff_I,
  y = ~n_shard,
  type = "scatter",
  mode = "markers",
  color = ~diff_G,
  colors = viridisLite::viridis(100),
  marker = list(
    size = 9,
    colorbar = list(title = "Unprotected<br>priority difference")
  ),
  text = ~hover_txt,
  hovertemplate = "%{text}<extra></extra>"
)

p_int <- plotly::layout(
  p_int,
  xaxis = list(title = "Difference in weighted conservation priority"),
  yaxis = list(title = "Shared threatened species"),
  template = "simple_white"
)

p_int



plot_df <- filtered_df |>
  st_drop_geometry() |>
  mutate(
    # total weighted priority (protected + unprotected)
    I_A = 1 * (p1A + p4A) +
      2 * (p2A + p5A) +
      3 * (p3A + p6A),
    
    I_B = 1 * (p1B + p4B) +
      2 * (p2B + p5B) +
      3 * (p3B + p6B),
    
    diff_I = abs(I_A - I_B),
    
    # weighted unprotected priority
    G_A = 1 * p4A + 2 * p5A + 3 * p6A,
    G_B = 1 * p4B + 2 * p5B + 3 * p6B,
    
    diff_G = abs(G_A - G_B),
    
    # weighted protected priority
    P_A = 1 * p1A + 2 * p2A + 3 * p3A,
    P_B = 1 * p1B + 2 * p2B + 3 * p3B,
    
    diff_P = abs(P_A - P_B)
  ) |>
  left_join(
    transb_species |>
      st_drop_geometry() |>
      select(seg_id, n_shard),
    by = "seg_id"
  )

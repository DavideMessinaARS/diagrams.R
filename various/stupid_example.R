start_point_ax = 0
start_point_ay = 0
start_point_bx = 2
start_point_by = 2
lenght_a = 2
height_a = 3
lenght_b = 2
height_b = 5
mid_point_ax_lenght = start_point_ax + lenght_a/2
mid_point_ay_height = start_point_ay + height_a/2
mid_point_bx_lenght = start_point_bx + lenght_b/2
mid_point_by_height = start_point_by + height_b/2
vector_x_coord_a = c(mid_point_ax_lenght, start_point_ax, mid_point_ax_lenght, lenght_a + start_point_ax)
vector_x_coord_b = c(mid_point_bx_lenght, start_point_bx, mid_point_bx_lenght, lenght_b + start_point_bx)
vector_y_coord_a = c(start_point_ay, mid_point_ay_height, height_a + start_point_ay, mid_point_ay_height)
vector_y_coord_b = c(start_point_by, mid_point_by_height, height_b + start_point_by, mid_point_by_height)
directions = c("up", "left", "down", "right")
matrix_a = data.frame(vector_x_coord_a, vector_y_coord_a, directions)
colnames(matrix_a) = c("x_a", "y_a", "direction_a")
matrix_b = data.frame(vector_x_coord_b, vector_y_coord_b, directions)
colnames(matrix_b) = c("x_b", "y_b", "direction_b")
final_df = tidyr::crossing(matrix_a, matrix_b)
  # mutate(x = (x_a - x_b)^2, y = (y_a - y_b)^2, z = sqrt(x + y)) %>%
select(final_df, x_a, y_a)
final_df = final_df %>%
  mutate(z = proxy::dist(select(final_df, x_a, y_a), select(final_df, x_b, y_b), pairwise = TRUE)) %>%
  filter(z == min(z))
direction_a = final_df$direction_a[1]
direction_b = final_df$direction_b[1]

a<-data.frame(x=1:10,y=1:10)
test_fun<-function(z){
  mean.x<-mean(z$x)
  nm <-deparse(substitute(z))
  print(nm)
  return(mean.x)}

test_fun(a)
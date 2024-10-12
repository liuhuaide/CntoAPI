# 安装所需包
install.packages("httr")
install.packages("ggmap")
install.packages("ggplot2")

# 加载包
library(httr)
library(ggmap)
library(ggplot2)

# 获取 API 的 URL
base_url <- "http://tile.stamen.com/toner/"

# 使用httr请求地图数据 (示例请求zoom级别为12, x=655, y=1582的瓦片)
response <- GET(paste0(base_url, "12/655/1582.png"))

# 检查请求状态
if (status_code(response) == 200) {
  print("成功获取瓦片地图数据")
} else {
  stop("请求失败，无法获取数据")
}

location <- c(lon = -122.4194, lat = 37.7749) # 旧金山的经纬度

# 使用 get_stadiamap 获取瓦片地图
stadia_map <- get_stadiamap(bbox = c(left = -123, bottom = 37.6, right = -122, top = 37.9),
                            zoom = 12, maptype = "stamen_toner")  # 修改为合法的 maptype

# 使用 ggmap 绘制地图
ggmap(stadia_map)


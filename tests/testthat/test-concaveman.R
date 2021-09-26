points = filter_poi(megaplot, Z > 2)
set.seed(42)
i = sample(1:npoints(points), 5000)
points = points[i]
points = lidR:::coordinates(points)

n = 342

test_that("concaveman works", {

# Has been sucessfully tested on Solaris with rhub...
# I'm tired to fix stuff on Solaris...
skip_on_os("solaris")

pts = points
hull <- concaveman(pts)
expect_equal(dim(hull), c(n,2))
expect_equivalent(hull[1,], hull[n,])

hull <- concaveman(pts, length_threshold = 10)
expect_equal(dim(hull), c(175,2))
expect_equivalent(hull[1,], hull[175,])

# It works with 3 points
pts = points[1:3,]
hull <- concaveman(pts)
expect_equal(dim(hull), c(4,2))
expect_equivalent(hull[1,], hull[4,])

# It does not work with < 3 points
pts = points[1:2,]
expect_error(concaveman(pts))

# It works with matrix
pts = as.matrix(points)
hull <- concaveman(pts)
expect_equal(dim(hull), c(n,2))
expect_equivalent(hull[1,], hull[n,])

# It works with vectors
x = points$X
y = points$Y
hull <- concaveman(x, y)
expect_equal(dim(hull), c(n,2))
expect_equivalent(hull[1,], hull[n,])

# Large concavity result in convex hull
hull <- concaveman(points, concavity = 1e8)
expect_equal(dim(hull), c(29,2))
expect_equivalent(hull[1,], hull[29,])
})

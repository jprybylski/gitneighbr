test_that(".port_is_open detects a bound port and a free one", {
  # base R's serverSocket() binds every interface, not just loopback (spec
  # §18.4: "no test binds a public network interface"), so this is skipped
  # defensively even though the bind is closed immediately.
  skip_on_cran()
  free_port <- .find_free_port()
  expect_false(.port_is_open("127.0.0.1", free_port))

  con <- serverSocket(free_port)
  on.exit(close(con), add = TRUE)
  expect_true(.port_is_open("127.0.0.1", free_port))
})

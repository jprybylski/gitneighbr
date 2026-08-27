test_that(".port_is_open detects a bound port and a free one", {
  free_port <- .find_free_port()
  expect_false(.port_is_open("127.0.0.1", free_port))

  con <- serverSocket(free_port)
  on.exit(close(con), add = TRUE)
  expect_true(.port_is_open("127.0.0.1", free_port))
})

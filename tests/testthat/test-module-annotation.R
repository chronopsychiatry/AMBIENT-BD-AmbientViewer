sessions <- data.frame(
  id = c("A", "B", "C", "D", "E"),
  annotation = c("Annot1", "Annot1", "", "", "Annot2"),
  .data_type = "somnofy_v2"
)

epochs <- data.frame(
  session_id = c("A", "A", "A", "I", "I", "C", "D", "E", "F", "G", "H", "B", "J"),
  epoch_data = c(1, 10, 20, 2, 3, 4, 5, 6, 7, 8, 9, 10, 5),
  .data_type = "somnofy_v2"
)

test_that("annotate epochs from sessions works", {
  session_colnames <- get_session_colnames(sessions)
  epoch_colnames <- get_epoch_colnames(epochs)

  result <- annotate_epochs_from_sessions(sessions, epochs, session_colnames, epoch_colnames)

  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), nrow(epochs))
  expect_equal(result$annotation[1:3], c("Annot1", "Annot1", "Annot1"))
  expect_equal(result$annotation[4:7], c("", "", "", ""))
})

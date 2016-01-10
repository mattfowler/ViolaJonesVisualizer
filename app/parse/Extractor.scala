package parse

trait Extractor[T] {
  def extract() : T
}

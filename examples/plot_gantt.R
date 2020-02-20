library(aitoaEvaluate);

aitoa.plot.gantt(list(
  list( list(job=11L,start=0L,end=68L),
        list(job=0L,start=83L,end=155L),
        list(job=8L,start=155L,end=194L),
        list(job=14L,start=250L,end=297L),
        list(job=1L,start=297L,end=320L),
        list(job=9L,start=320L,end=340L)
  ),
  list( list(job=3L,start=0L,end=68L),
        list(job=2L,start=68L,end=118L),
        list(job=12L,start=211L,end=271L)
  ),
  list( list(job=14L,start=0L,end=24L),
        list(job=13L,start=45L,end=137L),
        list(job=6L,start=137L,end=228L),
        list(job=4L,start=228L,end=262L)
  ),
  list( list(job=2L,start=118L,end=211L),
        list(job=1L,start=211L,end=254L),
        list(job=11L,start=254L,end=314L)
  ),
  list( list(job=8L,start=0L,end=35L),
        list(job=13L,start=35L,end=45L),
        list(job=3L,start=135L,end=234L),
        list(job=0L,start=234L,end=264L),
        list(job=2L,start=291L,end=308L)
  ),
  list( list(job=14L,start=24L,end=53L),
        list(job=13L,start=137L,end=160L),
        list(job=2L,start=211L,end=291L)
  ),
  list( list(job=1L,start=0L,end=19L),
        list(job=6L,start=19L,end=90L),
        list(job=0L,start=155L,end=229L),
        list(job=3L,start=234L,end=294L),
        list(job=13L,start=294L,end=340L)
  ),
  list( list(job=0L,start=0L,end=8L),
        list(job=4L,start=8L,end=92L),
        list(job=3L,start=92L,end=135L),
        list(job=7L,start=135L,end=225L),
        list(job=10L,start=225L,end=324L)
  ),
  list( list(job=5L,start=0L,end=60L),
        list(job=7L,start=60L,end=122L),
        list(job=1L,start=122L,end=195L),
        list(job=14L,start=195L,end=250L),
        list(job=6L,start=250L,end=315L)
  ),
  list( list(job=0L,start=8L,end=83L),
        list(job=14L,start=83L,end=132L),
        list(job=12L,start=132L,end=211L),
        list(job=9L,start=211L,end=269L),
        list(job=8L,start=269L,end=343L)
  )
), job.name.start = 1L, machine.name.prefix = "M", machine.name.start = 1L,
xlab=NA_character_, ylab=NA_character_);

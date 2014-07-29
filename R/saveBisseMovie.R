saveBisseMovie <-
function(q0=0.0,q1=0.555,b0=5.199,b1=2.933,d0=5.433,d1=2.319) {
  oopt=ani.options(nmax=500,interval=0.1)
  saveMovie(bisse.ani(q0=q0,q1=q1,b0=b0,b1=b1,d0=d0,d1=d1),convert='convert',outdir="/Users/bomeara/Desktop/",movietype = "gif",width=600, height=600,interval=0.5)
  ani.options(oopt)
  ani.stop()
}

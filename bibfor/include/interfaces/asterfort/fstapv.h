        interface
          subroutine fstapv(nbpt,fn,t,offset,fnmoyt,fnmoyc,fnrmst,&
     &fnrmsc,fnmax,fnmin,fmaxmo,fminmo,sfn,sfn2,tchoc,nbmaxr,nbminr)
            integer :: nbpt
            real(kind=8) :: fn(*)
            real(kind=8) :: t(*)
            real(kind=8) :: offset
            real(kind=8) :: fnmoyt
            real(kind=8) :: fnmoyc
            real(kind=8) :: fnrmst
            real(kind=8) :: fnrmsc
            real(kind=8) :: fnmax
            real(kind=8) :: fnmin
            real(kind=8) :: fmaxmo
            real(kind=8) :: fminmo
            real(kind=8) :: sfn
            real(kind=8) :: sfn2
            real(kind=8) :: tchoc
            integer :: nbmaxr
            integer :: nbminr
          end subroutine fstapv
        end interface

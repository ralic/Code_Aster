        interface
          subroutine fstat0(nbpt,fn,offset,fnmoyt,fnmoyc,fnrmst,fnrmsc&
     &,fnmax,fnmin,fmaxmo,fminmo,nbmaxr,nbminr)
            integer :: nbpt
            real(kind=8) :: fn(*)
            real(kind=8) :: offset
            real(kind=8) :: fnmoyt
            real(kind=8) :: fnmoyc
            real(kind=8) :: fnrmst
            real(kind=8) :: fnrmsc
            real(kind=8) :: fnmax
            real(kind=8) :: fnmin
            real(kind=8) :: fmaxmo
            real(kind=8) :: fminmo
            integer :: nbmaxr
            integer :: nbminr
          end subroutine fstat0
        end interface

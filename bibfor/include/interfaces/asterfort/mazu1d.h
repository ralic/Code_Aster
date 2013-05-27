        interface
          subroutine mazu1d(ee,mazars,sigm,varm,epsm,deps,esout,sigp,&
     &varp,option)
            real(kind=8) :: ee
            real(kind=8) :: mazars(*)
            real(kind=8) :: sigm
            real(kind=8) :: varm(*)
            real(kind=8) :: epsm
            real(kind=8) :: deps
            real(kind=8) :: esout
            real(kind=8) :: sigp
            real(kind=8) :: varp(*)
            character(*) :: option
          end subroutine mazu1d
        end interface

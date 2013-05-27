        interface
          subroutine nmcb1d(e0,labord,sigm,varm,epsm,deps,esout,sigp,&
     &varp,crit,option)
            real(kind=8) :: e0
            real(kind=8) :: labord(*)
            real(kind=8) :: sigm
            real(kind=8) :: varm(*)
            real(kind=8) :: epsm
            real(kind=8) :: deps
            real(kind=8) :: esout
            real(kind=8) :: sigp
            real(kind=8) :: varp(*)
            real(kind=8) :: crit(*)
            character(*) :: option
          end subroutine nmcb1d
        end interface

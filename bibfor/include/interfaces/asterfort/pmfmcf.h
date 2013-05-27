        interface
          subroutine pmfmcf(ip,nbgf,nbfib,nugf,sdcomp,crit,option,npg,&
     &instam,instap,icdmat,nbvalc,defam,defap,varim,varimp,contm,defm,&
     &defp,epsm,modf,sigf,varip,isecan,codret)
            integer :: ip
            integer :: nbgf
            integer :: nbfib
            integer :: nugf(*)
            character(len=24) :: sdcomp(*)
            real(kind=8) :: crit(*)
            character(len=16) :: option
            integer :: npg
            real(kind=8) :: instam
            real(kind=8) :: instap
            integer :: icdmat
            integer :: nbvalc
            real(kind=8) :: defam(*)
            real(kind=8) :: defap(*)
            real(kind=8) :: varim(*)
            real(kind=8) :: varimp(*)
            real(kind=8) :: contm(*)
            real(kind=8) :: defm(*)
            real(kind=8) :: defp(*)
            real(kind=8) :: epsm
            real(kind=8) :: modf(*)
            real(kind=8) :: sigf(*)
            real(kind=8) :: varip(*)
            integer :: isecan
            integer :: codret
          end subroutine pmfmcf
        end interface

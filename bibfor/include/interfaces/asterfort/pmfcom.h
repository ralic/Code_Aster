        interface
          subroutine pmfcom(kpg,option,compor,crit,nf,instam,instap,&
     &npg,nspg,icdmat,nbvalc,defam,defap,varim,varimp,contm,defm,ddefp,&
     &epsm,modf,sigf,varip,isecan,codret)
            integer :: nbvalc
            integer :: nf
            integer :: kpg
            character(len=16) :: option
            character(len=24) :: compor(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            integer :: npg
            integer :: nspg
            integer :: icdmat
            real(kind=8) :: defam(*)
            real(kind=8) :: defap(*)
            real(kind=8) :: varim(nbvalc*nf)
            real(kind=8) :: varimp(nbvalc*nf)
            real(kind=8) :: contm(nf)
            real(kind=8) :: defm(nf)
            real(kind=8) :: ddefp(nf)
            real(kind=8) :: epsm
            real(kind=8) :: modf(nf)
            real(kind=8) :: sigf(nf)
            real(kind=8) :: varip(nbvalc*nf)
            integer :: isecan
            integer :: codret
          end subroutine pmfcom
        end interface

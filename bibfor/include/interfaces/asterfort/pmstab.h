        interface
          subroutine pmstab(sigm,sigp,epsm,deps,nbvari,vim,vip,iforta,&
     &instam,instap,iter,nbpar,nompar,table,vr,igrad,valimp,imptgt,&
     &dsidep,nomvi,nbvita)
            real(kind=8) :: sigm(6)
            real(kind=8) :: sigp(6)
            real(kind=8) :: epsm(9)
            real(kind=8) :: deps(9)
            integer :: nbvari
            real(kind=8) :: vim(*)
            real(kind=8) :: vip(*)
            integer :: iforta
            real(kind=8) :: instam
            real(kind=8) :: instap
            integer :: iter
            integer :: nbpar
            character(len=16) :: nompar(*)
            character(len=8) :: table
            real(kind=8) :: vr(*)
            integer :: igrad
            real(kind=8) :: valimp(9)
            integer :: imptgt
            real(kind=8) :: dsidep(*)
            character(len=8) :: nomvi(*)
            integer :: nbvita
          end subroutine pmstab
        end interface

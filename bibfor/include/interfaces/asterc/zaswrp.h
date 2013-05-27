        interface
          subroutine zaswrp(numa,modele,nvar,ndef,nunit,instam,instap,&
     &nvarcm,nomvar,varplu,varmoi,varref,epsm,deps,sigm,vim,nopt,angeul,&
     &sigp,vip,dsidep,codret)
            integer(kind=4) :: numa
            integer(kind=4) :: modele
            integer(kind=4) :: nvar
            integer(kind=4) :: ndef
            integer(kind=4) :: nunit
            real(kind=8) :: instam
            real(kind=8) :: instap
            integer(kind=4) :: nvarcm
            character(*) :: nomvar
            real(kind=8) :: varplu(*)
            real(kind=8) :: varmoi(*)
            real(kind=8) :: varref(*)
            real(kind=8) :: epsm(*)
            real(kind=8) :: deps(*)
            real(kind=8) :: sigm(*)
            real(kind=8) :: vim(*)
            integer(kind=4) :: nopt
            real(kind=8) :: angeul(*)
            real(kind=8) :: sigp(*)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(*)
            integer(kind=4) :: codret
          end subroutine zaswrp
        end interface

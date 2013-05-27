        interface
          subroutine rcstoc(nommat,nomrc,nbobj,valr,valc,valk,nbr,nbc,&
     &nbk)
            character(len=8) :: nommat
            character(len=16) :: nomrc
            integer :: nbobj
            real(kind=8) :: valr(*)
            complex(kind=8) :: valc(*)
            character(len=8) :: valk(*)
            integer :: nbr
            integer :: nbc
            integer :: nbk
          end subroutine rcstoc
        end interface

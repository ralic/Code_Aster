        interface
          subroutine giecma(nfic,trouve,nbele,nomobj,tymail,nbno,ecrma&
     &,icoma)
            integer :: nfic
            logical :: trouve
            integer :: nbele
            character(len=8) :: nomobj
            character(len=8) :: tymail
            integer :: nbno
            logical :: ecrma(*)
            integer :: icoma
          end subroutine giecma
        end interface

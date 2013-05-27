        interface
          subroutine decod2(rec,irec,ifield,itype,ilu,rlu,trouve)
            character(*) :: rec(20)
            integer :: irec
            integer :: ifield
            integer :: itype
            integer :: ilu
            real(kind=8) :: rlu
            logical :: trouve
          end subroutine decod2
        end interface

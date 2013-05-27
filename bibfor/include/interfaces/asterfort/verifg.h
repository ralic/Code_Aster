        interface
          subroutine verifg(fami,npg,nspg,poum,imate,compor,ndim,epsth&
     &,iret)
            character(*) :: fami
            integer :: npg
            integer :: nspg
            character(*) :: poum
            integer :: imate
            character(*) :: compor
            integer :: ndim
            real(kind=8) :: epsth(*)
            integer :: iret
          end subroutine verifg
        end interface

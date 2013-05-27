        interface
          subroutine tldlg3(metrez,renum,istop,lmat,ildeb,ilfin,ndigit&
     &,ndeci,isingu,npvneg,iret,solvop)
            character(*) :: metrez
            character(*) :: renum
            integer :: istop
            integer :: lmat
            integer :: ildeb
            integer :: ilfin
            integer :: ndigit
            integer :: ndeci
            integer :: isingu
            integer :: npvneg
            integer :: iret
            character(len=19) :: solvop
          end subroutine tldlg3
        end interface

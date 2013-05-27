        interface
          subroutine enelpg(fami,iadmat,instan,igau,repere,xyzgau,&
     &compor,f,sigma,nbvari,vari,enelas)
            character(len=4) :: fami
            integer :: iadmat
            real(kind=8) :: instan
            integer :: igau
            real(kind=8) :: repere(7)
            real(kind=8) :: xyzgau(3)
            character(len=16) :: compor(*)
            real(kind=8) :: f(3,3)
            real(kind=8) :: sigma(6)
            integer :: nbvari
            real(kind=8) :: vari(*)
            real(kind=8) :: enelas
          end subroutine enelpg
        end interface

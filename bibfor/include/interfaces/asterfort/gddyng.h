        interface
          subroutine gddyng(kp,nno,en,x0sk,rmkm1,rmk,omkm1,ompkm1,omk,&
     &ompk,x0sec,rgmkm,rgmk,omgkm,ompgkm,omgk,ompgk)
            integer :: kp
            integer :: nno
            real(kind=8) :: en(3,2)
            real(kind=8) :: x0sk(3,3)
            real(kind=8) :: rmkm1(3,3)
            real(kind=8) :: rmk(3,3)
            real(kind=8) :: omkm1(3,3)
            real(kind=8) :: ompkm1(3,3)
            real(kind=8) :: omk(3,3)
            real(kind=8) :: ompk(3,3)
            real(kind=8) :: x0sec(3)
            real(kind=8) :: rgmkm(3)
            real(kind=8) :: rgmk(3)
            real(kind=8) :: omgkm(3)
            real(kind=8) :: ompgkm(3)
            real(kind=8) :: omgk(3)
            real(kind=8) :: ompgk(3)
          end subroutine gddyng
        end interface

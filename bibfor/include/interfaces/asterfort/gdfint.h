        interface
          subroutine gdfint(kp,nno,ajacob,pjacob,en,enprim,x0pg,pn,pm,&
     &fint)
            integer :: kp
            integer :: nno
            real(kind=8) :: ajacob
            real(kind=8) :: pjacob
            real(kind=8) :: en(3,2)
            real(kind=8) :: enprim(3,2)
            real(kind=8) :: x0pg(3)
            real(kind=8) :: pn(3)
            real(kind=8) :: pm(3)
            real(kind=8) :: fint(6,3)
          end subroutine gdfint
        end interface

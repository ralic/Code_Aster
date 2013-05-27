        interface
          subroutine burini(nmat,materd,materf,timed,timef,nvi,vind,nr&
     &,yd,deps,dy)
            integer :: nvi
            integer :: nmat
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: vind(nvi)
            integer :: nr
            real(kind=8) :: yd(*)
            real(kind=8) :: deps(6)
            real(kind=8) :: dy(*)
          end subroutine burini
        end interface

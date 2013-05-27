        interface
          subroutine rc32sn(typz,lieu,numsip,pi,mi,numsiq,pj,mj,seisme&
     &,mse,snij)
            character(*) :: typz
            character(len=4) :: lieu
            integer :: numsip
            real(kind=8) :: pi
            real(kind=8) :: mi(*)
            integer :: numsiq
            real(kind=8) :: pj
            real(kind=8) :: mj(*)
            logical :: seisme
            real(kind=8) :: mse(*)
            real(kind=8) :: snij
          end subroutine rc32sn
        end interface

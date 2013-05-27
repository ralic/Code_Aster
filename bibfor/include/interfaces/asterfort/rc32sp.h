        interface
          subroutine rc32sp(typz,lieu,numsip,pi,mi,numsiq,pj,mj,seisme&
     &,mse,spij,typeke,spmeca,spther)
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
            real(kind=8) :: spij(2)
            real(kind=8) :: typeke
            real(kind=8) :: spmeca(2)
            real(kind=8) :: spther(2)
          end subroutine rc32sp
        end interface

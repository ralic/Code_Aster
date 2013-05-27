        interface
          subroutine rc32s0(option,mm,pr,mse,sigun,nbinst,sth,snp)
            integer :: nbinst
            character(len=4) :: option
            real(kind=8) :: mm(*)
            real(kind=8) :: pr
            real(kind=8) :: mse(*)
            real(kind=8) :: sigun(*)
            real(kind=8) :: sth(6*nbinst)
            real(kind=8) :: snp
          end subroutine rc32s0
        end interface

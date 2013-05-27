        interface
          subroutine tbutnu(motfac,iocc,nomjv,nbinst,nomtab,prec,crit)
            character(len=16) :: motfac
            integer :: iocc
            character(*) :: nomjv
            integer :: nbinst
            character(*) :: nomtab
            real(kind=8) :: prec
            character(len=8) :: crit
          end subroutine tbutnu
        end interface

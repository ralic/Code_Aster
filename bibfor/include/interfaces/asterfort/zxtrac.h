        interface
          subroutine zxtrac(interp,prec,crit,nbinst,ti,temps,y,neq,&
     &xtract,ier)
            character(*) :: interp
            real(kind=8) :: prec
            character(*) :: crit
            integer :: nbinst
            real(kind=8) :: ti(*)
            real(kind=8) :: temps
            complex(kind=8) :: y(*)
            integer :: neq
            complex(kind=8) :: xtract(*)
            integer :: ier
          end subroutine zxtrac
        end interface

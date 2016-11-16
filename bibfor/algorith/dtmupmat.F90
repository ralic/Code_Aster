subroutine dtmupmat(sd_dtm_, sd_int_, buffdtm, buffint, nlcase,&
                    reinteg)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: hassan.berro at edf.fr
!
! dtmupmat : Update the stiffness K and Damping C-matrices for the current step
!            This is necessary in particular for gyroscopic effects where the
!            stifness and damping terms depend on time-variable rotation speeds
!            and accelerations.
!
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/coefmo.h"
#include "asterfort/dtmbuff.h"
#include "asterfort/dtmcase_coder.h"
#include "asterfort/dtmforc.h"
#include "asterfort/dtmfext.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtminivec.h"
#include "asterfort/dtmsav.h"
#include "asterfort/fointe.h"
#include "asterfort/intbackup.h"
#include "asterfort/intbuff.h"
#include "asterfort/intget.h"
#include "asterfort/intinivec.h"
#include "asterfort/intsav.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nlget.h"
#include "asterfort/pmavec.h"
#include "asterfort/prmama.h"
#include "asterfort/rrlds.h"
#include "asterfort/r8inir.h"
#include "asterfort/trlds.h"
#include "asterfort/vecini.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
!
!   -0.1- Input/output arguments
    character(len=*), intent(in)  :: sd_dtm_
    character(len=*), intent(in)  :: sd_int_
    integer, pointer              :: buffdtm(:)
    integer, pointer              :: buffint(:)
    integer, optional             :: nlcase
    integer, optional, intent(out):: reinteg
!
!   -0.2- Local variables
    aster_logical    :: mdiag, kdiag, cdiag
    integer          :: iret, nbmode, exgyro
    integer          :: exrigi, exrigy, isvvar, im, jm
    integer          :: ind, lvec, lev, i, j
    integer          :: nr, ind2, nlcase0, nbdof, ii
    integer          :: jj, iret2
    real(kind=8)     :: temps, dt0, vrotin, arotin, df
    real(kind=8)     :: k_added, epsi, dt, ratio
    real(kind=8)     :: prec, t2, dotpr, magsq, delta10
    real(kind=8)     :: delta20, coeff, c_added
    character(len=7) :: casek7
    character(len=8) :: sd_dtm, sd_int, sd_nl, foncv, fonca
    character(len=24):: kadd_jv, fadd_jv, cadd_jv
!
    integer, pointer      :: buffnl(:)  => null()
    integer, pointer      :: dk_add_ind(:)  => null()

    real(kind=8), pointer :: amorf(:)  => null()
    real(kind=8), pointer :: rigif(:)  => null()
    real(kind=8), pointer :: amor(:)   => null()
    real(kind=8), pointer :: rigi(:)   => null()
    real(kind=8), pointer :: gyogen(:) => null()
    real(kind=8), pointer :: rgygen(:) => null()
    real(kind=8), pointer :: kgen(:)   => null()
    real(kind=8), pointer :: agen(:)   => null()
   
    real(kind=8), pointer :: nlsav1(:)    => null()
    real(kind=8), pointer :: nlsav2(:)    => null()
    real(kind=8), pointer :: depl2(:)      => null()
    real(kind=8), pointer :: vite2(:)      => null()
    real(kind=8), pointer :: acce2(:)      => null()
    real(kind=8), pointer :: fext2(:)      => null()
    real(kind=8), pointer :: depl1(:)      => null()
    real(kind=8), pointer :: vite1(:)      => null()
    real(kind=8), pointer :: fext1(:)      => null()
    real(kind=8), pointer :: depl0(:)      => null()
    real(kind=8), pointer :: ddepl(:)      => null()
    real(kind=8), pointer :: dvite(:)      => null()
    real(kind=8), pointer :: k_add(:)      => null()
    real(kind=8), pointer :: k_add0(:)     => null()
    real(kind=8), pointer :: k_add_fact(:) => null()
    real(kind=8), pointer :: c_add(:)      => null()
    real(kind=8), pointer :: c_add0(:)     => null()
    real(kind=8), pointer :: f_add(:)      => null()
    real(kind=8), pointer :: f_add0(:)     => null()
    real(kind=8), pointer :: mgen0(:)      => null()
    real(kind=8), pointer :: kgen0(:)      => null()
    real(kind=8), pointer :: agen0(:)      => null()
    real(kind=8), pointer :: fext(:)      => null()
    real(kind=8), pointer :: amor_temp(:)      => null()
!
#define m0(row,col) mgen0((row-1)*nbmode+col)
#define k(row,col) kgen((row-1)*nbmode+col)
#define k0(row,col) kgen0((row-1)*nbmode+col)
#define k_a(row,col) k_add((row-1)*nbmode+col)
#define k_a0(row,col) k_add0((row-1)*nbmode+col)
#define k_a_f(row,col) k_add_fact((row-1)*nbdof+col)
#define c(row,col) agen((row-1)*nbmode+col)
#define c0(row,col) agen0((row-1)*nbmode+col)
#define c_a(row,col) c_add((row-1)*nbmode+col)

!
!   0 - Initializations
!
    sd_dtm = sd_dtm_
    sd_int = sd_int_
    epsi   = r8prem()
    if (present(reinteg)) reinteg = 0
!
!   1 - Update the stiffness matrix in the event of a change in state of some
!       non linearity 
    if (present(nlcase)) then
        call intget(sd_int, DEPL, iocc=1, lonvec=nbmode, buffer=buffint)
!       --- Check whether this nl-case has already been calculated
!           TO DO : create a proper data structure for matrices indexed by nl-case
!           TO DO : maximum number of nl : 24 because 2**24 has more than 7 digits
        call dtmcase_coder(nlcase, casek7)
        kadd_jv = sd_dtm // '.ADDED_K.'//casek7
        fadd_jv = sd_dtm // '.ADDED_F.'//casek7
        cadd_jv = sd_dtm // '.ADDED_C.'//casek7
        call jeexin(kadd_jv, iret)
        if (iret.gt.0) then
            call jeveuo(kadd_jv,'E', vr=k_add)
            call jeveuo(fadd_jv,'E', vr=f_add)
            call jeveuo(cadd_jv,'E', vr=c_add)
        else 
            call wkvect(kadd_jv,'V V R', nbmode*nbmode, vr=k_add)
            call wkvect(fadd_jv,'V V R', nbmode, vr=f_add)
            call wkvect(cadd_jv,'V V R', nbmode*nbmode, vr=c_add)
            if (nlcase.eq.0) then
                call vecini(nbmode*nbmode, 0.d0, k_add)
                call vecini(nbmode, 0.d0, f_add)
                call vecini(nbmode*nbmode, 0.d0, c_add)               
            end if
        end if

        call intget(sd_int, IND_ARCH, iscal=ind, buffer=buffint)
        call intget(sd_int, DEPL    , iocc =ind, vr=depl2, buffer=buffint)
        call intget(sd_int, STEP    , iocc =ind, rscal=dt0, buffer=buffint)
        ! write(*,*) 'dt0 = ', dt0
        call intget(sd_int, TIME    , iocc =ind, rscal=t2)
        call intget(sd_int, INDEX   , iocc =ind, iscal=ind2)
        AS_ALLOCATE(vr=depl1, size=nbmode)
        if (nlcase.gt.0) then
            call intget(sd_int, VITE    , iocc =ind, vr=vite2, buffer=buffint)
            call intget(sd_int, ACCE    , iocc =ind, vr=acce2, buffer=buffint)

            call dtmforc(sd_dtm, sd_int, ind, buffdtm, buffint, 1)
            call intget(sd_int, FORCE_EX, iocc =ind, vr=fext2, buffer=buffint)

            call dtmget(sd_dtm, _NL_SAVES, vr=nlsav1, buffer=buffdtm)

            call dtmget(sd_dtm, _SD_NONL  , kscal=sd_nl, buffer=buffdtm)
            call dtmget(sd_dtm, _NL_BUFFER, vi=buffnl, buffer=buffdtm)
            call nlget (sd_nl , _INTERNAL_VARS, vr=nlsav2, buffer=buffnl)
            call nlget (sd_nl , _INTERNAL_VARS, rvect=nlsav1, buffer=buffnl)
   
            dt = min(1.d-10,dt0*1.d-4)

            AS_ALLOCATE(vr=vite1, size=nbmode)
            AS_ALLOCATE(vr=fext1, size=nbmode)
            AS_ALLOCATE(vr=ddepl, size=nbmode)
            AS_ALLOCATE(vr=dvite, size=nbmode)

            call intget(sd_int, DEPL    , iocc=ind, rvect=depl1)
            call intget(sd_int, VITE    , iocc=ind, rvect=vite1)
            call intget(sd_int, FORCE_EX, iocc=ind, rvect=fext1)

            nr = 0
10          continue
            ! write(*,*) 'dt=', dt
    
            do i = 1, nbmode
                vite2(i) = vite1(i) + ( dt * acce2(i) )
                depl2(i) = depl1(i) + ( dt * vite2(i) )
            enddo
    
!           --- Insure that the change in displacements and velocities is larger than 
!               the machine's precision
            delta20 = 0.d0
            do i =1, nbmode
                delta20 = max(delta20,abs(vite2(i)-vite1(i)))
            end do
            if (delta20.le.1.d7*epsi) then
                dt = dt*10.d0
                nr = nr + 1
                goto 10
            end if

            delta20 = 0.d0
            do i =1, nbmode
                delta20 = max(delta20,abs(depl2(i)-depl1(i)))
            end do
            if (delta20.le.1.d7*epsi) then
                dt = dt*10.d0
                nr = nr + 1
                goto 10
            end if

            do i = 1, nbmode
                ddepl(i) = depl2(i)-depl1(i)
                dvite(i) = vite2(i)-vite1(i)
            end do

            ! write(*,*) 'ddepl=', ddepl
            ! write(*,*) 'dvite=', dvite

!           --- Calculation of the added stiffness and critical position for the NL
            call dcopy(nbmode, vite1, 1, vite2, 1)
            do i = 1, nbmode
!               --- Increment only mode #i with ddepl
                call dcopy(nbmode, depl1, 1, depl2, 1)
                depl2(i) = depl2(i)+ddepl(i)
!               --- Calculate the resulting force with this incrementation
                call dtmforc(sd_dtm, sd_int, ind, buffdtm, buffint, 1)
!               --- Determine the gradient matrix : df/dx, the non diagonal terms
!                   result from a modal coupling due to the non-linearity
                do j = 1, nbmode
                    df = fext1(j)-fext2(j)
                    ! write(*,*) 'i=',i,'j=',j,'df=', df
                    if ((abs(df).gt.1.d5*epsi).and. &
                        (min(abs(ddepl(i)),abs(ddepl(j))).gt.1.d5*epsi)) then
                        k_added  = df/ddepl(i)
                        k_a(i,j) = k_added
                    else
                        k_a(i,j) = 0.d0
                    end if
                end do
                call dcopy(nbmode, nlsav1, 1, nlsav2, 1)

!               --- Double check linearity for the diagonal terms
                if (abs(k_a(i,i)).gt.(1.d5*epsi)) then
                    df = fext1(i)-fext2(i)
!                   --- Increment mode #i with just 0.5*ddepl 
!                       (by decrementing the already incremented state 2)
                    depl2(i) = depl2(i)-0.5d0*ddepl(i)
!                   --- Calculate the resulting force with this -half- incrementation
                    call dtmforc(sd_dtm, sd_int, ind, buffdtm, buffint, 1)
                    ratio = (fext1(i)-fext2(i))/df
                    ! write(*,*) 'ratio = ', ratio
                    if ((ratio .lt. 0.49d0) .or. (ratio .gt. 0.51d0)) then
                        k_a(i,i) = 0.d0
                    end if
                end if
                call dcopy(nbmode, nlsav1, 1, nlsav2, 1)
            end do
            AS_DEALLOCATE(vr=ddepl)            

!           --- Calculation of the added damping
            call dcopy(nbmode, depl1, 1, depl2, 1)
            do i = 1, nbmode
!               --- Increment only mode #i with dvite
                call dcopy(nbmode, vite1, 1, vite2, 1)
                vite2(i) = vite2(i)+dvite(i)
!               --- Calculate the resulting force with this incrementation
                call dtmforc(sd_dtm, sd_int, ind, buffdtm, buffint, 1)
!               --- Determine the gradient matrix : df/dv, the non diagonal terms
!                   result from a modal coupling due to the non-linearity
                do j = 1, nbmode
                    df = fext1(j)-fext2(j)
                    if ((abs(df).gt.1.d5*epsi).and. &
                        (min(abs(dvite(i)),abs(dvite(j))).gt.1.d5*epsi)) then
                        c_added  = df/dvite(i)
                        c_a(i,j) = c_added
                    else
                        c_a(i,j) = 0.d0
                    end if
                end do
                call dcopy(nbmode, nlsav1, 1, nlsav2, 1)

!               --- Double check linearity for the diagonal terms
                if (abs(c_a(i,i)).gt.(1.d5*epsi)) then
                    df = fext1(i)-fext2(i)
!                   --- Increment mode #i with just 0.5*dvite 
!                       (by decrementing the already incremented state 2)
                    vite2(i) = vite2(i)-0.5d0*dvite(i)
!                   --- Calculate the resulting force with this -half- incrementation
                    call dtmforc(sd_dtm, sd_int, ind, buffdtm, buffint, 1)
                    ratio = (fext1(i)-fext2(i))/df
                    ! write(*,*) 'ratio = ', ratio
                    if ((ratio .lt. 0.49d0) .or. (ratio .gt. 0.51d0)) then
                        c_a(i,i) = 0.d0
                    end if
                end if
                call dcopy(nbmode, nlsav1, 1, nlsav2, 1)
            end do

!           --- Remove numerical errors resulting in non symetrical added matrices
            do i = 1, nbmode
                do j = i+1, nbmode
                    if (abs(k_a(i,j)).gt. 1.d3*epsi) then
                        ratio = abs(k_a(j,i)/k_a(i,j))
                        if ((ratio .lt. 0.99d0) .or. (ratio .gt. 1.01d0)) then
                            k_a(i,j) = 0.d0
                            k_a(j,i) = k_a(i,j)
                        else
                            k_a(i,j) = 0.5d0*(k_a(i,j)+k_a(j,i))
                            k_a(j,i) = k_a(i,j)
                        end if
                    else 
                        k_a(i,j) = 0.d0
                        k_a(j,i) = k_a(i,j)
                    end if
                    if (abs(c_a(i,j)).gt. 1.d3*epsi) then
                        ratio = abs(c_a(j,i)/c_a(i,j))
                        if ((ratio .lt. 0.99d0) .or. (ratio .gt. 1.01d0)) then
                            c_a(i,j) = 0.d0
                            c_a(j,i) = c_a(i,j)
                        else
                            c_a(i,j) = 0.5d0*(c_a(i,j)+c_a(j,i))
                            c_a(j,i) = c_a(i,j)
                        end if
                    else 
                        c_a(i,j) = 0.d0
                        c_a(j,i) = c_a(i,j)
                    end if
                end do
            end do

            call pmavec('ZERO', nbmode, k_add, depl1, f_add)
            call pmavec('CUMU', nbmode, c_add, vite1, f_add)

            AS_ALLOCATE(nbmode, vr=fext)
            call dtmfext(sd_dtm, t2, fext, buffdtm)
            do i=1, nbmode
                f_add(i) = f_add(i) + fext1(i) - fext(i)
            end do
            AS_DEALLOCATE(vr=fext)

            call dcopy(nbmode, depl1, 1, depl2, 1)
            call dcopy(nbmode, vite1, 1, vite2, 1)
            call dcopy(nbmode, fext1, 1, fext2, 1)

            AS_DEALLOCATE(vr=dvite)
            AS_DEALLOCATE(vr=vite1)
            AS_DEALLOCATE(vr=fext1)
        end if
    
        ! write(*,*) 'k_add = ', k_add
        ! write(*,*) 'f_add = ', f_add
        ! write(*,*) 'c_add = ', c_add

!       --- Calculate the critical coordinate of the point of non-linearity state change
!         * Calculate Xc where (K_add-K_add0) x Xc = (F_add-F_add0)
!         * K_add0 and F_add0 represent the stiffness and static force for the 
!           previous non-linear state   
!         * Xc is saved temporarly in depl0, then copied to depl1
        AS_ALLOCATE(vr=depl0     , size=nbmode)
        AS_ALLOCATE(vi=dk_add_ind, size=nbmode)
        AS_ALLOCATE(vr=k_add_fact, size=nbmode*nbmode)

        call dtmget(sd_dtm, _NL_CASE, iscal=nlcase0, buffer=buffdtm) 
        call dtmcase_coder(nlcase0 , casek7)
        kadd_jv = sd_dtm // '.ADDED_K.'//casek7
        fadd_jv = sd_dtm // '.ADDED_F.'//casek7
        cadd_jv = sd_dtm // '.ADDED_C.'//casek7       
        call jeexin(kadd_jv, iret)
        if (iret.gt.0) then
            call jeveuo(kadd_jv,'L',vr=k_add0)
            call jeveuo(fadd_jv,'L',vr=f_add0)
        else 
            call wkvect(kadd_jv, 'V V R', nbmode*nbmode, vr=k_add0)
            call wkvect(fadd_jv, 'V V R', nbmode, vr=f_add0)
            call wkvect(cadd_jv, 'V V R', nbmode*nbmode, vr=c_add0)
            call vecini(nbmode*nbmode, 0.d0, k_add0)
            call vecini(nbmode, 0.d0, f_add0)
            call vecini(nbmode*nbmode, 0.d0, c_add0)
            call jelibe(cadd_jv)
        end if

        call vecini(nbmode, 0.d0, depl0)
        nbdof = 0
        do i = 1, nbmode
            df = f_add(i)-f_add0(i)
            ! write(*,*) 'df=',df
            if ((abs(df).gt.100.d0*epsi).and.&
                (abs(k_a(i,i)-k_a0(i,i)).gt.100.d0*epsi)) then
                nbdof = nbdof + 1
                dk_add_ind(nbdof) = i
                depl0(i) = df
            end if
        end do 

!       --- Special treatment if the origin is the critical point 
        if (nbdof.eq.0) then
            call vecini(nbmode, 0.d0, depl1)
            goto 20
        end if

!       --- Remove the dof's that do not contribute to this nonlinearity, otherwise
!           the matrix k_add is extremely badly conditioned and thus non-reversible       
        do i = 1, nbdof
            ii = dk_add_ind(i)
            do j = i, nbdof
                jj = dk_add_ind(j)
                k_a_f(i,j) = k_a(ii,jj) - k_a0(ii,jj)
                k_a_f(j,i) = k_a(jj,ii) - k_a0(jj,ii)
            end do
        end do
        ! write(*,*) 'dk_add_ind = ', dk_add_ind
        ! write(*,*) 'k_add_fact = ', k_add_fact

!       --- Calculate Xc where   (K_add-K_add0) x Xc = (F_add-F_add0)
        call trlds(k_add_fact, nbdof, nbdof, iret)
        call rrlds(k_add_fact, nbdof, nbdof, depl0, 1)

!       --- Reconstruct Xc in full coordinates and save to depl1
        call vecini(nbmode, 0.d0, depl1)
        do i = 1, nbdof
            if (abs(depl0(i)).gt.100.d0*epsi) then
                depl1(dk_add_ind(i)) = depl0(i)
            end if
        end do

20      continue    
        AS_DEALLOCATE(vr=depl0)
        AS_DEALLOCATE(vi=dk_add_ind)
        AS_DEALLOCATE(vr=k_add_fact)

        call intget('&&INTBAK', DEPL, iocc=1, vr=depl0)

        ! write(*,*) 'depl2 = ', depl2
        ! write(*,*) 'depl1 = ', depl1
        ! write(*,*) 'depl0 = ', depl0

!       --- Calculate the projection of vector (01) on vector (02) in R^(nbmode)
!       --- First determine a scaling coefficient in order to avoid numerical errors
!           when manipulating differences in displacements for the projection 
        delta10 = 0.d0
        do i =1, nbmode
            delta10 = max(delta10,abs(depl1(i)-depl0(i)))
        end do
        if (delta10.le.1.d5*epsi) then
            ratio = 1.d0
        else 
            coeff = 1.d0/delta10

            magsq = 0.d0
            dotpr = 0.d0
            do i =1, nbmode
                delta10 = coeff * (depl1(i)-depl0(i))
                delta20 = coeff * (depl2(i)-depl0(i))
                dotpr   = dotpr + delta10 * delta20
                magsq   = magsq + (delta20)**2
            end do
            ratio = dotpr/magsq
        end if

        if (ratio.gt.1.d0) ratio = 1.d0
        ! write(*,*) 'ratio = ', ratio

        prec = 1.d-2 
        if    (((ratio.gt.epsi).and.(abs(ratio-1.d0).gt.prec)) &
            .and.((dt0*ratio).gt.1.d-10)) then

            call intbackup('&&INTBAK', sd_int)
            call dtmget(sd_dtm, _NL_SAVE0, rvect=nlsav2, buffer=buffdtm)

            nullify(buffint)
            call intget(sd_int, IND_ARCH, iscal=lev)
            call intbuff(sd_int, buffint, level=lev)

            dt = dt0*ratio

            call intsav(sd_int, STEP , 1, iocc=1, rscal=dt)
            call intsav(sd_int, TIME , 1, iocc=1, rscal=t2-dt0)
            call intsav(sd_int, INDEX, 1, iocc=1, iscal=ind2-1)

            ! write(*,*) 'old dt = ', dt0
            ! write(*,*) 'new dt = ', dt
            reinteg = 1
        end if

        AS_DEALLOCATE(vr=depl1)

        if (reinteg.eq.1) goto 999

        mdiag = .false.
        call dtmget(sd_dtm, _MASS_FUL, lonvec=iret, buffer=buffdtm)
        if (iret.gt.0) then
            call dtmget(sd_dtm, _MASS_FUL, vr=mgen0, buffer=buffdtm)
        else
            call dtmget(sd_dtm, _MASS_DIA, vr=mgen0, buffer=buffdtm)
            mdiag = .true.
        end if

        kdiag = .false.
        call dtmget(sd_dtm, _RIGI_FUL, lonvec=iret, buffer=buffdtm)
        if (iret.gt.0) then
            call dtmget(sd_dtm, _RIGI_FUL, vr=kgen0, buffer=buffdtm)
        else
            call dtmget(sd_dtm, _RIGI_DIA, vr=kgen0, buffer=buffdtm)
            kdiag = .true.
        end if

        cdiag = .false.
        call dtmget(sd_dtm, _AMOR_FUL, lonvec=iret, buffer=buffdtm)
        if (iret.gt.0) then
            call dtmget(sd_dtm, _AMOR_FUL, vr=agen0, buffer=buffdtm)
        else
            call dtmget(sd_dtm, _AMOR_DIA, vr=agen0, buffer=buffdtm)
            cdiag = .true.
        end if

        if (nbmode.eq.1) then
            call intget(sd_int, RIGI_DIA, iocc=1, vr=kgen, buffer=buffint)
            call intget(sd_int, AMOR_DIA, iocc=1, vr=agen, buffer=buffint)
            kgen(1) = kgen0(1) + k_a(1,1)
            agen(1) = mgen0(1)*agen0(1) + c_a(1,1)
        else 
            call intget(sd_int, RIGI_FUL, iocc=1, lonvec=iret, buffer=buffint)
            if (iret.eq.0) then
                call intinivec(sd_int, RIGI_FUL, nbmode*nbmode, iocc=1, vr=kgen)
            else 
                call intget(sd_int, RIGI_FUL, iocc=1, vr=kgen, buffer=buffint)
            end if

            call intget(sd_int, AMOR_FUL, iocc=1, lonvec=iret, buffer=buffint)
            if (iret.eq.0) then
                call intinivec(sd_int, AMOR_FUL, nbmode*nbmode, iocc=1, vr=agen)
                call intget(sd_int, IND_ARCH, iscal=lev)
            else 
                call intget(sd_int, AMOR_FUL, iocc=1, vr=agen, buffer=buffint)
            end if

            nullify(buffint)
            call intget(sd_int, IND_ARCH, iscal=lev)
            call intbuff(sd_int, buffint, level=lev)

            if (kdiag) then
                do i = 1, nbmode
                    k(i,i) = kgen0(i) + k_a(i,i)
                    do j = i+1, nbmode
                        k(i,j) = k_a(i,j)
                        k(j,i) = k_a(j,i)
                    end do
                end do
            else
                do i = 1, nbmode
                    do j = i, nbmode
                        k(i,j) = k0(i,j) + k_a(i,j)
                        k(j,i) = k0(j,i) + k_a(j,i)
                    end do
                end do
            end if

            if (cdiag) then
                if (mdiag) then
                    do i = 1, nbmode
                        c(i,i) = agen0(i)*mgen0(i) + c_a(i,i)
                        do j = i+1, nbmode
                            c(i,j) = c_a(i,j)
                            c(j,i) = c_a(j,i)
                        end do
                    end do
                else 
                    do i = 1, nbmode
                        c(i,i) = agen0(i)*m0(i,i) + c_a(i,i)
                        do j = i+1, nbmode
                            c(i,j) = c_a(i,j)
                            c(j,i) = c_a(j,i)
                        end do
                    end do
                end if

            else
                do i = 1, nbmode
                    do j = i, nbmode
                        c(i,j) = c0(i,j) + c_a(i,j)
                        c(j,i) = c0(j,i) + c_a(j,i)
                    end do
                end do
            end if
        end if

        ! write(*,*) 'kgen=', kgen
        ! write(*,*) 'agen=', agen

        call dtmsav(sd_dtm, _NL_CASE, 1, iscal=nlcase)

    end if

!   --------------------------------------------------------------------------------------
!   1 - First determine whether a full update is needed, or just a referral of 
!       the matrices from a preceding step
!   --------------------------------------------------------------------------------------

!
    call dtmget(sd_dtm, _GYRO_FUL, lonvec=exgyro, buffer=buffdtm)
    if (exgyro.gt.0) then
        call dtmget(sd_dtm, _NB_MODES, iscal=nbmode, buffer=buffdtm)
!
!       --- Is a full damping matrix given for the calculations ?
        call dtmget(sd_dtm, _AMOR_FUL, lonvec=iret, buffer=buffdtm)
        if (iret.eq.0) then
            call dtminivec(sd_dtm, _AMOR_FUL, nbmode*nbmode, vr=amor)
            call dtmget(sd_dtm, _AMOR_DIA, lonvec=iret2, buffer=buffdtm)
            if (iret2.gt.0) then
                call dtmget(sd_dtm, _AMOR_DIA, vr=agen, buffer=buffdtm)
                AS_ALLOCATE(vr=amor_temp, size=nbmode*nbmode)
                call r8inir(nbmode*nbmode, 0.d0, amor_temp, 1)
                do i = 1, nbmode
                    amor_temp((i-1)*nbmode+i) = agen(i)
                end do
                call dtmget(sd_dtm, _MASS_FUL, vr=mgen0, buffer=buffdtm)
                call prmama(1, mgen0, nbmode, nbmode, nbmode,&
                            amor_temp, nbmode, nbmode, nbmode, amor,&
                            nbmode, nbmode, nbmode, iret)
                AS_DEALLOCATE(vr=amor_temp)
            end if
            call dtmbuff(sd_dtm, buffdtm)
        end if

!
!       --- Renforce the checking on the existence of RIGI_FULL and RIGY_FUL
        call dtmget(sd_dtm, _RIGI_FUL, lonvec=exrigi, buffer=buffdtm)
        ASSERT(exrigi.gt.0)
!
        call dtmget(sd_dtm, _RIGY_FUL, lonvec=exrigy, buffer=buffdtm)
        ASSERT(exrigi.gt.0)
!
!       --- Prepare the full damping and stifness matrices in the integration sd

        call intget(sd_int, AMOR_FUL, lonvec=iret, buffer=buffint)
        if (iret.eq.0) then
            call intinivec(sd_int, AMOR_FUL, nbmode*nbmode, iocc=1, vr=amorf)
            call intinivec(sd_int, RIGI_FUL, nbmode*nbmode, iocc=1, vr=rigif)
            call intget(sd_int, IND_ARCH, iscal=lev)
            call intbuff(sd_int, buffint, level=lev)
        else
            call intget(sd_int, AMOR_FUL, iocc=1, vr=amorf, buffer=buffint)
            call intget(sd_int, RIGI_FUL, iocc=1, vr=rigif, buffer=buffint)
        end if

        call dtmget(sd_dtm, _AMOR_FUL, vr=amor, buffer=buffdtm)
        call dtmget(sd_dtm, _RIGI_FUL, vr=rigi, buffer=buffdtm)
        call dtmget(sd_dtm, _GYRO_FUL, vr=gyogen, buffer=buffdtm)
        call dtmget(sd_dtm, _RIGY_FUL, vr=rgygen, buffer=buffdtm)

!       --- Is the rotation speed variable ?
        call dtmget(sd_dtm, _V_ROT_F, lonvec=isvvar)
        if (isvvar.gt.0) then
!           --- If yes, then extract v_rot and a_rot then combine the matrices
            call dtmget(sd_dtm, _V_ROT_F, kscal=foncv, buffer=buffdtm)
            call dtmget(sd_dtm, _A_ROT_F, kscal=fonca, buffer=buffdtm)
            call intget(sd_int, TIME, iocc=1, rscal=temps)

            call fointe('F ', foncv, 1, ['INST'], [temps],&
                        vrotin, iret)
            arotin = 0.d0
            if (fonca(1:1) .ne. ' ') then
                call fointe('F ', fonca, 1, ['INST'], [temps],&
                            arotin, iret)
            end if
            do im = 1, nbmode
                do jm = 1, nbmode
                    ind = jm + nbmode*(im-1)
                    amorf(ind) = amor(ind) + vrotin * gyogen(ind)
                    rigif(ind) = rigi(ind) + arotin * rgygen(ind)
                enddo
            enddo
        else
!           --- If not, then just copy the matrices
            call dcopy(nbmode*nbmode, amor, 1, amorf, 1)
            call dcopy(nbmode*nbmode, rigi, 1, rigif, 1)
        endif

        call intsav(sd_int, MAT_UPDT, 1, iscal=1, buffer=buffint)
!
    end if

!   --- Simple calculation with constant matrices, no action required, just
!       intialize integration sd with the matrices info if ind = INDEX(index)=0
    call intget(sd_int, INDEX, iocc=1, iscal=ind)
    if (ind.eq.0) then
!        --- Mass
!        call dtmget(sd_dtm, _MASS_FUL, lonvec=lvec)
!        if (lvec.gt.0) then
!           call intinivec(sd_int, MASS_FUL, lvec, iocc=1, vr=mgen)
!           call dtmget(sd_dtm, _MASS_FUL, rvect=mgen)
!        endif
!        call dtmget(sd_dtm, _MASS_DIA, lonvec=lvec)
!        call intinivec(sd_int, MASS_DIA, lvec, iocc=1, vr=mgen)
!        call dtmget(sd_dtm, _MASS_DIA, rvect=mgen)
!
!       --- Stifness
        call dtmget(sd_dtm, _RIGI_FUL, lonvec=lvec)
        if (lvec.gt.0) then
            call intinivec(sd_int, RIGI_FUL, lvec, iocc=1, vr=kgen)
            call dtmget(sd_dtm, _RIGI_FUL, rvect=kgen)
        endif
        call dtmget(sd_dtm, _RIGI_DIA, lonvec=lvec)
        call intinivec(sd_int, RIGI_DIA, lvec, iocc=1, vr=kgen)
        call dtmget(sd_dtm, _RIGI_DIA, rvect=kgen)
!
!       --- Damping
        call dtmget(sd_dtm, _AMOR_FUL, lonvec=lvec)
        if (lvec.gt.0) then
            call intinivec(sd_int, AMOR_FUL, lvec, iocc=1, vr=agen)
            call dtmget(sd_dtm, _AMOR_FUL, rvect=agen)
        endif
        call dtmget(sd_dtm, _AMOR_DIA, lonvec=lvec)
        call intinivec(sd_int, AMOR_DIA, lvec, iocc=1, vr=agen)
        call dtmget(sd_dtm, _AMOR_DIA, rvect=agen)

        call intget(sd_int, IND_ARCH, iscal=lev)
        call intbuff(sd_int, buffint, level=lev)
    endif

999 continue
!
end subroutine

subroutine mdptem(nbmode, masgen, pulsat, nbchoc, dt,&
                  dtmax, dtmin, tinit, tfin, nbpas,&
                  ier, lisins)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8depi.h"
#include "asterc/r8prem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nlget.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nbchoc, nbpas, ier, nbmode
    real(kind=8) :: masgen(*), pulsat(*)
    real(kind=8) :: dt, tinit, tfin, dtmax, dtmin
    character(len=24) :: lisins
! ----------------------------------------------------------------------
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
!
!     VERIFICATION ET CALCUL DU PAS DE TEMPS
!     ------------------------------------------------------------------
! IN  : NBMODE : NOMBRE DE MODES
! IN  : MASGEN : MASSES GENERALISEES
! IN  : PULSAT : PULSATIONS MODALES
! IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
! IN  : DPLMOD : TABLEAU DES DEPL MODAUX AUX NOEUDS DE CHOC
! IN  : PARCHO : TABLEAU DES PARAMETRES DE CHOC
! IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
! OUT : DT     : PAS DE TEMPS
! OUT : TINIT  : TEMPS INITIAL
! OUT : TFIN   : TEMPS FINAL
! OUT : NBPAS  : NOMBRE DE PAS CALCULE (INITIAL NON COMPRIS)
! OUT : IER    : CODE RETOUR
! OUT : LISINS : LISTE DES INSTANTS POUR L'ARCHIVAGE
! ----------------------------------------------------------------------
    integer :: ic, i, j, iveri, ibid, iret
    integer :: jbint,  jvale, jvalr, jinst
    integer :: n1, n2, n3, n4, n5, n6, nr, nt, nni
    integer :: nbgrpa, nbinst, nbinsr, numef, nbordr
    integer :: vali
    real(kind=8) :: dts, dtu, knorm, ktang, r8bid
    real(kind=8) :: valr(3)
    real(kind=8) :: zero, deuxpi, dti, dtp, eps
    character(len=8) :: veripa, nomres, tran, li, sd_nl
    character(len=16) :: typres, nomcmd, method
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: k, n7, nume, nbcomps, nbno
    integer, pointer :: ordr(:) => null()
    real(kind=8), pointer :: lpas(:) => null()

    real(kind=8), pointer :: dplmod(:) => null()
    real(kind=8), pointer :: dplmod1(:) => null()
    real(kind=8), pointer :: dplmod2(:) => null()

!-----------------------------------------------------------------------
    call getres(nomres, typres, nomcmd)
    sd_nl = '&&OP29NL'
!
    tinit = 0.d0
    ier = 0
    iveri = 0
    zero = 0.d0
    deuxpi = r8depi()
    eps = r8prem()*1.d5
    dts = 1.d10
    dtu = 1.d+10
    dtmax = 1.d+10
    call getvtx('SCHEMA_TEMPS', 'SCHEMA', iocc=1, scal=method, nbret=n1)
!
!     VERIFICATION DE PRESENCE DU PAS SI ADAPT OU RUNGE-KUTTA
!     (DEJA FAIT DANS MDVERI POUR ITMI)
    if ((method(1:5).eq.'ADAPT') .or. (method(1:5).eq.'RUNGE')) then
        call getvr8('INCREMENT', 'PAS', iocc=1, nbval=0, nbret=ibid)
        if (ibid .eq. 0) then
            call utmess('F', 'ALGORITH3_11')
        endif
    endif
!
!     RECUPERATION de TINIT
    call getvid('ETAT_INIT', 'RESULTAT', iocc=1, scal=tran, nbret=nr)
    if (nr .ne. 0) then
        call getvis('ETAT_INIT', 'NUME_ORDRE', iocc=1, scal=nume, nbret=nni)
        if (nni .eq. 0) then
            call getvr8('ETAT_INIT', 'INST_INIT', iocc=1, scal=tinit, nbret=nt)
            if (nt .eq. 0) then
                call jeveuo(tran//'           .DISC', 'L', jinst)
                call jelira(tran//'           .DISC', 'LONUTI', nbinst)
                tinit = zr(jinst+nbinst-1)
            endif
        else
            call jeveuo(tran//'           .ORDR', 'L', vi=ordr)
            call jelira(tran//'           .ORDR', 'LONUTI', nbordr)
            do 100 i = 1, nbordr
                if (ordr(i) .eq. nume) goto 101
100          continue
            call utmess('F', 'ALGORITH3_36', sk=tran)
101          continue
            call jeveuo(tran//'           .DISC', 'L', jinst)
            tinit = zr(jinst+i-1)
        endif
    else
        call getvid('INCREMENT', 'LIST_INST', iocc=1, scal=li, nbret=nt)
        if (nt .ne. 0) then
            call jeveuo(li//'           .BINT', 'L', jbint)
            tinit = zr (jbint)
        else
            call getvr8('INCREMENT', 'INST_INIT', iocc=1, scal=tinit, nbret=n2)
        endif
    endif
!
!     RECUPERATION de DT et TFIN
    call getvid('INCREMENT', 'LIST_INST', iocc=1, scal=li, nbret=nt)
    if (nt .ne. 0) then
        if (method(1:5) .eq. 'RUNGE') then
            call utmess('F', 'ALGORITH3_9')
        endif
        call jeveuo(li//'           .BINT', 'L', jbint)
        call jeveuo(li//'           .LPAS', 'L', vr=lpas)
        call jeveuo(li//'           .VALE', 'L', jvale)
        call jelira(li//'           .VALE', 'LONUTI', nbinst)
        call jelira(li//'           .NBPA', 'LONUTI', nbgrpa)
!
        if (nbgrpa .eq. 1) then
            dtu = lpas(1)
            tfin = zr (jbint+1)
        else
!              CHOIX DTU PLUS PETIT DE LA LISTE
            dtu = lpas(1)
            do 32 j = 1, nbgrpa-1
                dtu = min(dtu,lpas(1+j))
32          continue
!              TEST PAS DE TEMPS CONSTANT SI PLUSIEURS INTERVALLES
            do 33 i = 1, nbgrpa-1
                if ((abs(lpas(1+i)-dtu)) .ge. (1.d-6*dtu)) then
                    call utmess('F', 'ALGORITH3_18')
                endif
33          continue
            tfin = zr (jbint+nbgrpa)
        endif
        call getvis('INCREMENT', 'NUME_FIN', iocc=1, scal=numef, nbret=n1)
        if (n1 .eq. 0) then
            call getvr8('INCREMENT', 'INST_FIN', iocc=1, scal=tfin, nbret=n1)
            if (n1 .eq. 0) goto 99
        else
            call jeveuo(li//'           .VALE', 'L', jvalr)
            call jelira(li//'           .VALE', 'LONUTI', nbinsr)
            if (numef .ge. nbinsr) goto 99
            tfin = zr(jvalr+numef)
        endif
    else
        call getvr8('INCREMENT', 'INST_FIN', iocc=1, scal=tfin, nbret=n3)
        call getvr8('INCREMENT', 'PAS', iocc=1, scal=dtu, nbret=n4)
        if (abs(dtu).le.eps) then
            call utmess('F', 'ALGORITH3_12')
        endif
    endif
99  continue
    call getvtx('INCREMENT', 'VERI_PAS', iocc=1, scal=veripa, nbret=n5)
    if (veripa .eq. 'OUI') iveri = 1
!
    do i = 1, nbmode
        if (abs(pulsat(i)).gt.eps) then
            dti = deuxpi / pulsat(i)
            dts = min( dts , dti )
        endif
    end do
!
    if (nbchoc .gt. 0) then
!
        do 20 i = 1, nbchoc
            knorm = 0.d0
            ktang = 0.d0

            call nlget(sd_nl, _STIF_NORMAL, iocc=i, lonvec=iret)
            if (iret.gt.0) call nlget(sd_nl, _STIF_NORMAL, iocc=i, rscal=knorm)
            call nlget(sd_nl, _RIGI_TANGENTIAL, iocc=i, lonvec=iret)
            if (iret.gt.0) call nlget(sd_nl, _RIGI_TANGENTIAL, iocc=i, rscal=ktang)

            call nlget(sd_nl, _MODAL_DEPL_NO1, iocc=i, vr=dplmod1)

            nbno = 1
            call nlget(sd_nl, _MODAL_DEPL_NO2, iocc=i, lonvec=iret)
            if (iret.gt.0) then 
                nbno = 2
                call nlget(sd_nl, _MODAL_DEPL_NO2, iocc=i, vr=dplmod2)
            end if

            nbcomps = size(dplmod1)/nbmode

            ic = 1
            dplmod => dplmod1
24          continue
            do 22 j = 1, nbmode
                if (abs(pulsat(j)).le.eps) goto 22
                if (abs(masgen(j)).le.eps) goto 22
!
                if (abs(knorm).gt.eps) then
                    dti = deuxpi / sqrt(pulsat(j)**2+knorm*dplmod((j-1)*nbcomps+1)**2 / masgen(j))
                    dts = min(dts, dti)
                    dti = deuxpi / sqrt(pulsat(j)**2+knorm*dplmod((j-1)*nbcomps+2)**2 / masgen(j))
                    dts = min(dts, dti)
                    dti = deuxpi / sqrt(pulsat(j)**2+knorm*dplmod((j-1)*nbcomps+3)**2 / masgen(j))
                    dts = min(dts, dti)
                endif
                if (abs(ktang).gt.eps) then
                    dti = deuxpi / sqrt(pulsat(j)**2+ktang*dplmod((j-1)*nbcomps+1)**2 / masgen(j))
                    dts = min(dts, dti)
                    dti = deuxpi / sqrt(pulsat(j)**2+ktang*dplmod((j-1)*nbcomps+2)**2 / masgen(j))
                    dts = min(dts, dti)
                    dti = deuxpi / sqrt(pulsat(j)**2+ktang*dplmod((j-1)*nbcomps+3)**2 / masgen(j))
                    dts = min(dts, dti)
                endif
22          continue
            if (ic .eq. 5) goto 20
            if (nbno.eq.2) then
                ic = 5
                nullify(dplmod)
                dplmod => dplmod2
                goto 24
            endif
20      continue
    endif
!
    if (method .eq. 'DEVOGE') then
        dtp = dts / 10.d0
        dt = min( dtp , dtu )
    else if (method .eq. 'NEWMARK') then
        dtp = dts / 10.d0
        dt = min( dtp , dtu )
    else if (method(1:5).eq.'RUNGE') then
        dt = dtu
    else
!      CASE METHOD .EQ. 'DIFF_CENTRE' OR OTHER
        dtp = dts / 20.d0
        dt = min( dtp , dtu )
    endif
!
    nbpas = nint( ( tfin - tinit ) / dt )
!
    if (dtu .gt. dt) then
        if (method .eq. 'NEWMARK') then
            valr (1) = dtu
            valr (2) = dt
            call utmess('A', 'ALGORITH16_14', nr=2, valr=valr)
            dt = dtu
            nbpas = nint( ( tfin - tinit ) / dt )
        else if (iveri.eq.1 .and. method(1:5) .ne. 'ADAPT') then
            ier = ier + 1
            valr (1) = dtu
            valr (2) = dt
            vali = nbpas
            call utmess('E', 'ALGORITH16_15', si=vali, nr=2, valr=valr)
        else
            valr (1) = dtu
            valr (2) = dt
            call utmess('A+', 'ALGORITH16_16', nr=2, valr=valr)
            if (iveri .ne. 1) then
                call utmess('A+', 'ALGORITH16_89')
            endif
            vali = nbpas
            call utmess('A', 'ALGORITH16_17', si=vali)
            dt = dtu
            nbpas = nint( ( tfin - tinit ) / dt )
        endif
    endif

!
!     SI LA METHODE N'EST PAS ADAPT, RUNGE-KUTTA OU ITMI:
!     SI LIST_INST DANS ARCHIVAGE ALORS:
!     BESOIN D'UNE LISTE DES INSTANTS DE CALCUL POUR LA CREATION
!     DE LA LISTE D'ARCHIVAGE DANS DYARCH.F
    call getvid('ARCHIVAGE', 'LIST_INST', iocc=1, nbval=0, nbret=n6)
    call getvr8('ARCHIVAGE', 'INST', iocc=1, nbval=0, nbret=n7)
    if (n6 .ne. 0 .or. n7 .ne. 0) then
!
        call wkvect(lisins, 'V V R', nbpas+1, jvale)
        j=0
        zr(jvale)=tinit
        do 30 k = 1, nbpas
            j = j + 1
            zr(jvale+j) = tinit + k*dt
30      continue
    endif
!
!     GESTION DU PAS MAXIMAL POUR SCHEMA ADAPT OU RUNGE-KUTTA
    if (method(1:5) .eq. 'ADAPT' .or. (method(1:5).eq.'RUNGE') .or. (method(1:6).eq.'DEVOGE') ) then
        call getvr8('SCHEMA_TEMPS', 'PAS_MAXI', iocc=1, scal=r8bid, nbret=n6)
        if (n6 .ne. 0) then
            call getvr8('SCHEMA_TEMPS', 'PAS_MAXI', iocc=1, scal=dtmax, nbret=n6)
            if (dtmax .gt. (dts/20.d0)) then
                valr (1) = dtmax
                valr (2) = dt
                call utmess('A', 'DYNAMIQUE_17', nr=2, valr=valr)
            endif
            if (( dtmax .lt. dtu )) then
                valr (1) = dtmax
                valr (2) = dtu
                call utmess('E', 'DYNAMIQUE_15', nr=2, valr=valr)
            endif
!
        else
            dtmax = dts / 20.d0
            if (dts .ge. 1.d10) then
                call utmess('A', 'DYNAMIQUE_16', sr=dtmax)
            endif
        endif
    endif
!
!     GESTION DU PAS MINI POUR SCHEMA RUNGE-KUTTA
    if (method(1:5) .eq. 'RUNGE') then
        call getvr8('SCHEMA_TEMPS', 'PAS_MINI', iocc=1, scal=r8bid, nbret=n6)
        if (n6 .ne. 0) then
            call getvr8('SCHEMA_TEMPS', 'PAS_MINI', iocc=1, scal=dtmin, nbret=n6)
        else
            dtmin = 1000.d0*r8prem()
        endif
    else
        dtmin = 1000.d0*r8prem()
    endif
!
end subroutine

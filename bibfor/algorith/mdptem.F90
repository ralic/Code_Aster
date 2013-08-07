subroutine mdptem(nbmode, masgen, pulsat, nbchoc, dplmod,&
                  parcho, noecho, dt, dts, dtu,&
                  dtmax, dtmin, tinit, tfin, nbpas,&
                  info, ier, lisins)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8depi.h"
#include "asterc/r8prem.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nbchoc, nbpas, info, ier, nbmode
    real(kind=8) :: masgen(*), pulsat(*), parcho(nbchoc, *)
    real(kind=8) :: dplmod(nbchoc, nbmode, *)
    real(kind=8) :: dt, tinit, tfin, dtmax, dts, dtu, dtmin
    character(len=8) :: noecho(nbchoc, *)
    character(len=24) :: lisins
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! OUT : DTS    : PAS DE TEMPS LIMITE (BASE ET CHOCS)
! OUT : DTU    : PAS DE TEMPS UTILISATEUR
! OUT : TINIT  : TEMPS INITIAL
! OUT : TFIN   : TEMPS FINAL
! OUT : NBPAS  : NOMBRE DE PAS CALCULE (INITIAL NON COMPRIS)
! OUT : IER    : CODE RETOUR
! OUT : LISINS : LISTE DES INSTANTS POUR L'ARCHIVAGE
! ----------------------------------------------------------------------
    integer :: ic, ia, i, j, iveri, ibid
    integer :: jbint, jlpas, jvale, jvalr, jinst, jordr
    integer :: n1, n2, n3, n4, n5, n6, nr, nt, nni
    integer :: nbgrpa, nbinst, nbinsr, numef, nbordr
    integer :: vali
    real(kind=8) :: knorm, ktang, r8bid
    real(kind=8) :: valr(3)
    real(kind=8) :: zero, deuxpi, dti, dtp, dta
    character(len=8) :: veripa, nomres, tran, li
    character(len=16) :: typres, nomcmd, method
    character(len=19) :: numarc
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: k, n7, nume
!-----------------------------------------------------------------------
    call getres(nomres, typres, nomcmd)
!
    tinit = 0.d0
    ier = 0
    iveri = 0
    zero = 0.d0
    deuxpi = r8depi()
    dts = 1.d10
    dtu = 1.d+10
    dtmax = 1.d+10
    lisins= ' '
    call getvtx('SCHEMA_TEMPS', 'SCHEMA', 1, iarg, 1,&
                method, n1)
!
!     VERIFICATION DE PRESENCE DU PAS SI ADAPT OU RUNGE-KUTTA
!     (DEJA FAIT DANS MDVERI POUR ITMI)
    if ((method(1:5).eq.'ADAPT') .or. (method(1:5).eq.'RUNGE')) then
        call getvr8('INCREMENT', 'PAS', 1, iarg, 0,&
                    dta, ibid)
        if (ibid .eq. 0) call u2mess('F', 'ALGORITH3_11')
    endif
!
!     RECUPERATION de TINIT
    call getvid('ETAT_INIT', 'RESULTAT', 1, iarg, 1,&
                tran, nr)
    if (nr .ne. 0) then
        call getvis('ETAT_INIT', 'NUME_ORDRE', 1, iarg, 1,&
                    nume, nni)
        if (nni .eq. 0) then
            call getvr8('ETAT_INIT', 'INST_INIT', 1, iarg, 1,&
                        tinit, nt)
            if (nt .eq. 0) then
                call jeveuo(tran//'           .DISC', 'L', jinst)
                call jelira(tran//'           .DISC', 'LONUTI', nbinst)
                tinit = zr(jinst+nbinst-1)
            endif
        else
            call jeveuo(tran//'           .ORDR', 'L', jordr)
            call jelira(tran//'           .ORDR', 'LONUTI', nbordr)
            do 100 i = 1, nbordr
                if (zi(jordr+i-1) .eq. nume) goto 101
100          continue
            call u2mesk('F', 'ALGORITH3_36', 1, tran)
101          continue
            call jeveuo(tran//'           .DISC', 'L', jinst)
            tinit = zr(jinst+i-1)
        endif
    else
        call getvid('INCREMENT', 'LIST_INST', 1, iarg, 1,&
                    li, nt)
        if (nt .ne. 0) then
            call jeveuo(li//'           .BINT', 'L', jbint)
            tinit = zr (jbint)
        else
            call getvr8('INCREMENT', 'INST_INIT', 1, iarg, 1,&
                        tinit, n2)
            if (n2 .eq. 0) then
                call u2mess('I', 'ALGORITH5_62')
            endif
        endif
    endif
!
!     RECUPERATION de DT et TFIN
    call getvid('INCREMENT', 'LIST_INST', 1, iarg, 1,&
                li, nt)
    if (nt .ne. 0) then
        if (method(1:5) .eq. 'RUNGE') then
            call u2mess('F', 'ALGORITH3_9')
        endif
        call jeveuo(li//'           .BINT', 'L', jbint)
        call jeveuo(li//'           .LPAS', 'L', jlpas)
        call jeveuo(li//'           .VALE', 'L', jvale)
        call jelira(li//'           .VALE', 'LONUTI', nbinst)
        call jelira(li//'           .NBPA', 'LONUTI', nbgrpa)
!
        if (nbgrpa .eq. 1) then
            dtu = zr (jlpas)
            tfin = zr (jbint+1)
        else
!              CHOIX DTU PLUS PETIT DE LA LISTE
            dtu = zr(jlpas)
            do 32 j = 1, nbgrpa-1
                dtu = min(dtu,zr(jlpas+j))
32          continue
!              TEST PAS DE TEMPS CONSTANT SI PLUSIEURS INTERVALLES
            do 33 i = 1, nbgrpa-1
                if ((abs(zr(jlpas+i)-dtu)) .ge. (1.d-6*dtu)) then
                    call u2mess('F', 'ALGORITH3_18')
                endif
33          continue
            tfin = zr (jbint+nbgrpa)
        endif
        call getvis('INCREMENT', 'NUME_FIN', 1, iarg, 1,&
                    numef, n1)
        if (n1 .eq. 0) then
            call getvr8('INCREMENT', 'INST_FIN', 1, iarg, 1,&
                        tfin, n1)
            if (n1 .eq. 0) goto 99
        else
            call jeveuo(li//'           .VALE', 'L', jvalr)
            call jelira(li//'           .VALE', 'LONUTI', nbinsr)
            if (numef .ge. nbinsr) goto 99
            tfin = zr(jvalr+numef)
        endif
    else
        call getvr8('INCREMENT', 'INST_FIN', 1, iarg, 1,&
                    tfin, n3)
        call getvr8('INCREMENT', 'PAS', 1, iarg, 1,&
                    dtu, n4)
        if (dtu .eq. 0.d0) call u2mess('F', 'ALGORITH3_12')
    endif
99  continue
    call getvtx('INCREMENT', 'VERI_PAS', 1, iarg, 1,&
                veripa, n5)
    if (veripa .eq. 'OUI') iveri = 1
!
    do 10 i = 1, nbmode
        if (pulsat(i) .ne. zero) then
            dti = deuxpi / pulsat(i)
            dts = min( dts , dti )
        endif
10  end do
!
    if (nbchoc .gt. 0) then
!
        do 20 i = 1, nbchoc
            knorm = parcho(i,2)
            ktang = parcho(i,4)
            ic = 1
            ia = 0
24          continue
            if (info .eq. 2) call u2mesg('I', 'ALGORITH16_92', 1, noecho(i, ic), 0,&
                                         0, 0, 0.d0)
            do 22 j = 1, nbmode
                if (pulsat(j) .eq. zero) goto 22
!
                if (knorm .ne. zero) then
                    dti = deuxpi / sqrt(pulsat(j)**2 + knorm * dplmod( i,j,1+ia)**2 / masgen(j))
                    dts = min(dts, dti)
                    dti = deuxpi / sqrt(pulsat(j)**2 + knorm * dplmod( i,j,2+ia)**2 / masgen(j))
                    dts = min(dts, dti)
                    dti = deuxpi / sqrt(pulsat(j)**2 + knorm * dplmod( i,j,3+ia)**2 / masgen(j))
                    dts = min(dts, dti)
                endif
                if (ktang .ne. zero) then
                    dti = deuxpi / sqrt(pulsat(j)**2 + ktang * dplmod( i,j,1+ia)**2 / masgen(j))
                    dts = min(dts, dti)
                    dti = deuxpi / sqrt(pulsat(j)**2 + ktang * dplmod( i,j,2+ia)**2 / masgen(j))
                    dts = min(dts, dti)
                    dti = deuxpi / sqrt(pulsat(j)**2 + ktang * dplmod( i,j,3+ia)**2 / masgen(j))
                    dts = min(dts, dti)
                endif
22          continue
            if (ic .eq. 5) goto 20
            if (noecho(i,9)(1:2) .eq. 'BI') then
                ic = 5
                ia = 3
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
    else if (method .eq. 'ITMI') then
        dt = dtu
        goto 9999
    else
!      CASE METHOD .EQ. 'EULER' OR OTHER
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
            call u2mesg('A', 'ALGORITH16_14', 0, ' ', 0,&
                        0, 2, valr)
            dt = dtu
            nbpas = nint( ( tfin - tinit ) / dt )
        else if (iveri.eq.1 .and. method(1:5) .ne. 'ADAPT') then
            ier = ier + 1
            valr (1) = dtu
            valr (2) = dt
            vali = nbpas
            call u2mesg('E', 'ALGORITH16_15', 0, ' ', 1,&
                        vali, 2, valr)
        else
            valr (1) = dtu
            valr (2) = dt
            call u2mesg('A+', 'ALGORITH16_16', 0, ' ', 0,&
                        0, 2, valr)
            if (iveri .ne. 1) call u2mess('A+', 'ALGORITH16_89')
            vali = nbpas
            call u2mesg('A', 'ALGORITH16_17', 0, ' ', 1,&
                        vali, 0, 0.d0)
            dt = dtu
            nbpas = nint( ( tfin - tinit ) / dt )
        endif
    endif
9999  continue
!
!     SI LA METHODE N'EST PAS ADAPT, RUNGE-KUTTA OU ITMI:
!     SI LIST_INST DANS ARCHIVAGE ALORS:
!     BESOIN D'UNE LISTE DES INSTANTS DE CALCUL POUR LA CREATION
!     DE LA LISTE D'ARCHIVAGE DANS DYARCH.F
    call getvid('ARCHIVAGE', 'LIST_INST', 1, iarg, 0,&
                numarc, n6)
    call getvr8('ARCHIVAGE', 'INST', 1, iarg, 0,&
                r8bid, n7)
    if (n6 .ne. 0 .or. n7 .ne. 0) then
        if ((method(1:5) .ne. 'ADAPT') .or. (method.ne.'ITMI') .or.&
            (method(1:5).ne.'RUNGE')) then
!
            call wkvect('&&OP0074.MD_JVALE', 'V V R', nbpas+1, jvale)
            j=0
            zr(jvale)=tinit
            do 30 k = 1, nbpas
                j = j + 1
                zr(jvale+j) = tinit + k*dt
30          continue
            lisins= '&&OP0074.MD_JVALE'
        endif
    endif
!
!     GESTION DU PAS MAXIMAL POUR SCHEMA ADAPT OU RUNGE-KUTTA
    if (method(1:5) .eq. 'ADAPT' .or. (method(1:5).eq.'RUNGE')) then
        call getvr8('INCREMENT', 'PAS_MAXI', 1, iarg, 1,&
                    r8bid, n6)
        if (n6 .ne. 0) then
            call getvr8('INCREMENT', 'PAS_MAXI', 1, iarg, 1,&
                        dtmax, n6)
            if (dtmax .gt. (dts/20.d0)) then
                valr (1) = dtmax
                valr (2) = dt
                call u2mesg('A', 'DYNAMIQUE_17', 0, ' ', 0,&
                            0, 2, valr)
            endif
            if (( dtmax .lt. dtu )) then
                valr (1) = dtmax
                valr (2) = dtu
                call u2mesg('E', 'DYNAMIQUE_15', 0, ' ', 0,&
                            0, 2, valr)
            endif
!
        else
            dtmax = dts / 20.d0
            if (dts .ge. 1.d10) call u2mesg('A', 'DYNAMIQUE_16', 0, ' ', 0,&
                                            0, 1, dtmax)
        endif
    endif
!
!     GESTION DU PAS MINI POUR SCHEMA RUNGE-KUTTA
    if (method(1:5) .eq. 'RUNGE') then
        call getvr8('INCREMENT', 'PAS_MINI', 1, iarg, 1,&
                    r8bid, n6)
        if (n6 .ne. 0) then
            call getvr8('INCREMENT', 'PAS_MINI', 1, iarg, 1,&
                        dtmin, n6)
        else
            dtmin = 1000.d0*r8prem()
        endif
    else
        dtmin = 1000.d0*r8prem()
    endif
!
end subroutine

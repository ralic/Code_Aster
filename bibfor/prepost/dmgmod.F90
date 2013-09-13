subroutine dmgmod(nomsym, nomsd, nomsd2, nommat, nbordr,&
                  jordr, jcoef, nbpt, ntcmp, numcmp,&
                  impr, vdomag)
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rcvale.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nommat
    character(len=16) :: nomsym
    character(len=19) :: nomsd, nomsd2
    real(kind=8) :: vdomag(*)
    integer :: nbpt, numcmp(*)
    integer :: ntcmp, impr, nbordr, jordr, jcoef
!       ----------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       CREATION D UN VECTEUR AUX NOEUDS/PG : AMPLITUDE MAX DE VIBRATION
!       METHODE CALCUL DU DOMMAGE UNITAIRE = /WOHLER
!       ----------------------------------------------------------------
!       IN     NOMSYM    NOM SYMBOLIQUE OPTION EQUI_GD
!              NOMSD     NOM SD RESULTAT STATIQUE
!              NOMSD2     NOM SD RESULTAT MODAL
!              NOMMAT    NOM DU CHAM_MATER
!              NBORDR    NOMBRE DE NUMEROS D'ORDRE
!              JORD      ADRESSE DE LA LISTE DES NUMEROS D'ORDRE
!              JCEIF      ADRESSE DE LA LISTE DES COEFFICIENTS
!              NBPT      NOMBRE DE POINTS DE CALCUL DU DOMMAGE
!              NTCMP     NOMBRE TOTAL DE COMPOSANTE OPTION EQUI_GD
!              NUMCMP    NUMERO(S) DE LA(DES) COMPOSANTE(S) DE EQUI_GD
!              IMPR      NIVEAU IMPRESSION
!       OUT    VDOMAG    VECTEUR DOMMAGE AUX POINTS
!       ----------------------------------------------------------------
!       ---------------------------------------------------------------
    character(len=8) :: nomrm(1), nompar, kcorre, nomfon
    character(len=10) :: nomphe
    character(len=19) :: chequi, chequ2(nbordr)
    character(len=24) :: nomdmg
    character(len=24) :: valk(3)
    integer :: icodre(1)
!
    real(kind=8) :: su, salt0, dmax, saltm, val(1)
    real(kind=8) :: valr(3), r8b, dmin, smax, coeff, r8min
!
    integer :: ipt, iord, icmp, nbr, nbk, nbc, nbf
    integer :: ivch, ivord, ivpt, ibid, ivalk
    integer :: numsym, ivch2, ivord2(nbordr), numord
    logical :: crit
!
! ---   VECTEURS DE TRAVAIL
!
!-----------------------------------------------------------------------
    integer :: ik, ivalf
!-----------------------------------------------------------------------
    r8b = 0.d0
    call jemarq()
!
    nomdmg = '&&OP0151.EQUI_GD'
    call wkvect(nomdmg, 'V V R', 2, ivpt)
!
    call getvtx(' ', 'CORR_SIGM_MOYE', scal=kcorre, nbret=ibid)
!
! --    VECTEUR DES NUMORD NOMS DE CHAMPS POUR L OPTION NOMSYM
!
    call jenonu(jexnom(nomsd//'.DESC', nomsym), numsym)
    if (numsym .eq. 0) then
        valk(1) = nomsym
        valk(2) = nomsd
        call u2mesk('F', 'PREPOST_51', 2, valk)
    endif
    call jeveuo(jexnum(nomsd//'.TACH', numsym), 'L', ivch)
!
    call jenonu(jexnom(nomsd2//'.DESC', nomsym), numsym)
    if (numsym .eq. 0) then
        valk(1) = nomsym
        valk(2) = nomsd2
        call u2mesk('F', 'PREPOST_51', 2, valk)
    endif
    call jeveuo(jexnum(nomsd2//'.TACH', numsym), 'L', ivch2)
!
! RECUPERATION PROPRIETES MATERIAUX
!
    nomrm(1) = 'SU'
    nompar = ' '
    call rcvale(nommat, 'RCCM', 0, nompar, [r8b],&
                1, nomrm, val, icodre(1), 2)
    if (icodre(1) .ne. 0) then
        valk(1) = 'SU'
        call u2mesk('F', 'FATIGUE1_88', 1, valk)
    endif
    su = val(1)
!
    nomphe = 'FATIGUE   '
    call jelira(nommat//'.'//nomphe//'.VALR', 'LONUTI', nbr)
    call jelira(nommat//'.'//nomphe//'.VALC', 'LONUTI', nbc)
    call jeveuo(nommat//'.'//nomphe//'.VALK', 'L', ivalk)
    call jelira(nommat//'.'//nomphe//'.VALK', 'LONUTI', nbk)
    nbf = (nbk-nbr-nbc)/2
    do 50 ik = 1, nbf
        if (zk8(ivalk-1+nbr+nbc+ik) .eq. 'WOHLER') then
            nomfon = zk8(ivalk-1+nbr+nbc+nbf+ik)
            call jeveuo(nomfon//'           .VALE', 'L', ivalf)
            salt0=zr(ivalf)
        endif
50  end do
!
    valr(1) = su
    valr(2) = salt0
    call u2mesg('I', 'FATIGUE1_87', 0, ' ', 0,&
                0, 2, valr)
!
    icmp = 1
    dmin = 1.d10
    smax= 0.d0
    crit = .false.
    r8min = r8miem()
!
! ---       CALCUL DU VECTEUR HISTOIRE DE LA EQUI_GD EN CE POINT
!
    chequi = zk24(ivch)(1:19)
    if (chequi .eq. ' ') then
        valk(1) = chequi
        valk(2) = nomsym
        valk(3) = nomsd
        call u2mesk('F', 'PREPOST_52', 3, valk)
    endif
    call jeveuo(chequi//'.CELV', 'L', ivord)
!
    do 11 iord = 1, nbordr
        numord = zi(jordr+iord-1)
        chequ2(iord) = zk24(ivch2+numord-1)(1:19)
        if (chequ2(iord) .eq. ' ') then
            valk(1) = chequ2(iord)
            valk(2) = nomsym
            valk(3) = nomsd2
            call u2mesk('F', 'PREPOST_52', 3, valk)
        endif
        call jeveuo(chequ2(iord)//'.CELV', 'L', ivord2(iord))
11  continue
!
! ---     BOUCLE SUR LES POINTS
!
    do 10 ipt = 1, nbpt
! -    STOCKAGE CONTRAINTES
        zr(ivpt) = zr(ivord+(ipt-1)*ntcmp+numcmp(icmp)-1)
        zr(ivpt+1) = 0.d0
        do 12 iord = 1, nbordr
            coeff = zr(jcoef+iord-1)
            zr(ivpt+1) = zr(ivpt+1) + coeff* abs(zr(ivord2(iord)+(ipt- 1)*ntcmp+numcmp(icmp)-1))
12      continue
!
        if (zr(ivpt) .gt. su) then
            if (impr .ge. 2) call u2mesg('I', 'FATIGUE1_80', 0, ' ', 0,&
                                         0, 0, valr)
            saltm = 0.d0
            crit = .true.
            smax = max(zr(ivpt) , smax)
        else if (zr(ivpt) .gt. 0.d0) then
            if (kcorre .eq. 'GOODMAN') then
                saltm = salt0 *(1 - zr(ivpt)/su )
            else if (kcorre.eq.'GERBER') then
                saltm = salt0 *(1 - (zr(ivpt)/su)**2 )
            endif
        else
            saltm = salt0
        endif
!
        if (abs(zr(ivpt+1)) .gt. r8min) then
            dmax = saltm / abs(zr(ivpt+1))
        else
            dmax = 1.d10
        endif
        if (dmax .lt. dmin) dmin = dmax
!
        vdomag(ipt) = dmax
        if (impr .ge. 2) then
            valr (1) = zr(ivpt)
            valr (2) = zr(ivpt+1)
            valr (3) = dmax
            call u2mesg('I', 'FATIGUE1_79', 0, ' ', 1,&
                        ipt, 3, valr)
        endif
!
10  continue
!
    if (crit) then
        valr (1) = smax
        valr (2) = su
        call u2mesg('A', 'FATIGUE1_83', 0, ' ', 0,&
                    0, 2, valr)
    endif
    call u2mesg('I', 'FATIGUE1_82', 0, ' ', 0,&
                0, 1, dmin)
!
!
    call jedema()
end subroutine

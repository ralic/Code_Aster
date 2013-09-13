subroutine fgvdmg(nomsym, nomsd, nommat, nomnap, nomfon,&
                  mexpic, mcompt, mdomag, nbord, nbpt,&
                  ntcmp, nbcmp, numcmp, impr, vdomag)
    implicit none
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/fgcota.h"
#include "asterfort/fgdomg.h"
#include "asterfort/fgpic2.h"
#include "asterfort/fgrain.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nommat, nomnap, nomfon
    character(len=16) :: nomsym
    character(len=19) :: nomsd
    character(len=*) :: mexpic, mcompt, mdomag
    real(kind=8) :: vdomag(*)
    integer :: nbpt, nbcmp, numcmp(*), nbord
    integer :: ntcmp, impr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       CREATION D UN VECTEUR AUX NOEUDS/PG  DE DOMMAGE
!       POUR LE MOMENT :
!       GRANDEUR 1D EQUIVALENTE (EQUI_GD)  = /VMIS_SG
!                                            /INVA_2_SG
!                                            (EQUI_ELNO/GA_SIGM/EPSI)
!       METHODE D EXTRACTION DES PICS      = RAINFLOW
!       METHODE DE COMPTAGE DES CYCLES     = RAINFLOW
!       METHODE CALCUL DU DOMMAGE UNITAIRE = /WOHLER
!                                            /MANSON_COFFIN
!                                            /TAHERI_MANSON
!                                            /TAHERI_MIXTE
!       METHODE DE CUMUL DU DOMMAGE        = LINEAIRE
!       ----------------------------------------------------------------
!       IN     NOMSYM    NOM SYMBOLIQUE OPTION EQUI_GD
!              NOMSD     NOM SD RESULTAT
!              NOMMAT    NOM DU CHAM_MATER
!              NOMNAP    NOM DE LA NAPPE POUR LOI TAHERI
!              NOMFON    NOM DE LA FONCTION POUR LOI TAHERI
!              MEXPIC    METHODE EXTRACTION DES PICS
!              MCOMPT    METHODE DE COMPTAGE DES CYCLES
!              MDOMAG    METHODE DE CALCUL DU DOMMAGE
!              NBORD     NOMBRE DE NUMEROS D'ORDRE
!              NBPT      NOMBRE DE POINTS DE CALCUL DU DOMMAGE
!              NTCMP     NOMBRE TOTAL DE COMPOSANTE OPTION EQUI_GD
!              NBCMP     NOMBRE DE COMPOSANTES DE EQUI_GD UTILISEES(1)
!              NUMCMP    NUMERO(S) DE LA(DES) COMPOSANTE(S) DE EQUI_GD
!              IMPR      NIVEAU IMPRESSION
!       OUT    VDOMAG    VECTEUR DOMMAGE AUX POINTS
!       ----------------------------------------------------------------
!       REMARQUE         DANS LE CAS OU IL Y A N COMPOSANTES POUR LA
!                        EQUI_GD , ON CALCULE LE DOMMAGE
!                        POUR CHAQUE COMPOSANTE ET ON 'NORME'
!       ----------------------------------------------------------------
!       ---------------------------------------------------------------
    character(len=8) :: kcmp
    character(len=19) :: chequi
    character(len=24) :: nomdmg, nompic
    character(len=24) :: nomitv, nomrtv
    character(len=24) :: valk(3)
!
    real(kind=8) :: dommag
    real(kind=8) :: valr(2)
!
    integer :: ipt, iord, icmp
    integer :: ivch, ivord, ivpic, ivitv, ivrtv, ivpt
    integer :: numsym, ibid
    integer :: vali
!
! ---   VECTEURS DE TRAVAIL
!
!-----------------------------------------------------------------------
    integer :: ivmax, ivmin, j, ncyc, npic
!-----------------------------------------------------------------------
    call jemarq()
    nomdmg = '&&OP0151.EQUI_GD'
    nompic = '&&OP0151.PICS'
    nomitv = '&&OP0151.ITRAV'
    nomrtv = '&&OP0151.RTRAV'
    call wkvect(nompic, 'V V R', nbord+2, ivpic)
    call wkvect(nomitv, 'V V I', nbord+2, ivitv)
    call wkvect(nomrtv, 'V V R', nbord+2, ivrtv)
    call wkvect(nomdmg, 'V V R', nbord, ivpt)
    call wkvect('&&OP0151.SIGMAX', 'V V R', nbord+2, ivmax)
    call wkvect('&&OP0151.SIGMIN', 'V V R', nbord+2, ivmin)
!
! --    VECTEUR DES NBORD NOMS DE CHAMPS POUR L OPTION NOMSYM
!
    call jenonu(jexnom(nomsd//'.DESC', nomsym), numsym)
    if (numsym .eq. 0) then
        valk(1) = nomsym
        valk(2) = nomsd
        call utmess('F', 'PREPOST_51', nk=2, valk=valk)
    endif
    call jeveuo(jexnum(nomsd//'.TACH', numsym), 'L', ivch)
!
! ---   BOUCLE SUR LES COMPOSANTES DE LA EQUI_GD
!
    do 50 icmp = 1, nbcmp
        if (impr .ge. 2) call codent(icmp, 'G', kcmp)
!
! ---     BOUCLE SUR LES POINTS
!
        do 10 ipt = 1, nbpt
            if (impr .ge. 2) then
                valk (1) = kcmp
                vali = ipt
                call utmess('I', 'PREPOST6_6', sk=valk(1), si=vali)
            endif
!
! ---       CALCUL DU VECTEUR HISTOIRE DE LA EQUI_GD EN CE POINT
!           BOUCLE SUR LES NBORD NUMEROS D ORDRE
!
            do 30 iord = 1, nbord
                chequi = zk24(ivch+iord-1)(1:19)
!
                if (chequi .eq. ' ') then
                    valk(1) = chequi
                    valk(2) = nomsym
                    valk(3) = nomsd
                    call utmess('F', 'PREPOST_52', nk=3, valk=valk)
                endif
!
                if ((icmp .eq. 1) .and. (ipt .eq. 1)) then
                    call jeveuo(chequi//'.CELV', 'L', ivord)
                    call jelira(chequi//'.CELV', 'LONMAX', ibid)
                    if (ibid .ne. (nbpt*ntcmp)) then
                        valk(2) = nomsym
                        vali = ntcmp
                        call utmess('F', 'FATIGUE1_78', sk=valk(2), si=vali)
                    endif
                endif
!
                call jeveuo(chequi//'.CELV', 'L', ivord)
!
! -         STOCKAGE COMPOSANTE NUMCMP(ICMP)
                zr(ivpt+iord-1) = zr(ivord+(ipt-1)*ntcmp+numcmp(icmp)- 1)
!
!
30          continue
!
! ---     POSSEDANT ENFIN LE VECTEUR HISTOIRE DE LA EQUI_GD EN CE POINT
!         ON VA POUVOIR CALCULER LE DOMMAGE RESULTANT EN UTILISANT :
!         METHODE D EXTRACTION DES PICS      = RAINFLOW
!         METHODE DE COMPTAGE DES CYCLES     = RAINFLOW
!         METHODE CALCUL DU DOMMAGE          = WOHLER_LINEAIRE
!
            if (mcompt .eq. 'RAINFLOW') then
                call fgpic2(mexpic, zr(ivrtv), zr(ivpt), nbord, zr(ivpic),&
                            npic)
                call fgrain(zr(ivpic), npic, zi(ivitv), ncyc, zr(ivmin),&
                            zr(ivmax))
            else if (mcompt(1:6).eq.'TAHERI') then
                call fgcota(nbord, zr(ivpt), ncyc, zr(ivmin), zr(ivmax))
            endif
            if (ncyc .eq. 0) then
                call utmess('F', 'FATIGUE1_77', si=ncyc)
            endif
!
            if (impr .ge. 2) then
                vali = nbord
                valr (1) = zr(ivpt)
                valr (2) = zr(ivpt+1)
                call utmess('I', 'PREPOST6_7', si=vali, nr=2, valr=valr)
                if (mcompt .eq. 'RAINFLOW') then
                    vali = npic
                    valr (1) = zr(ivpic)
                    valr (2) = zr(ivpic+1)
                    call utmess('I', 'PREPOST6_8', si=vali, nr=2, valr=valr)
                endif
                vali = ncyc
                call utmess('I', 'PREPOST6_9', si=vali)
                do 223 j = 1, ncyc
                    vali = j
                    valr (1) = zr(ivmax+j-1)
                    valr (2) = zr(ivmin+j-1)
                    call utmess('I', 'PREPOST6_10', si=vali, nr=2, valr=valr)
223              continue
            endif
!
! ---     CALCUL DU DOMMAGE AU POINT IPT ET STOCK DANS VECTEUR VDOMAG
!
            call fgdomg(mdomag, nommat, nomnap, nomfon, zr(ivmin),&
                        zr( ivmax), ncyc, dommag)
!
            vdomag(ipt) = dommag
            if (impr .ge. 2) then
                valr (1) = dommag
                call utmess('I', 'PREPOST6_11', sr=valr(1))
            endif
!
10      continue
!
50  end do
!
    call jedema()
end subroutine

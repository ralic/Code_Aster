subroutine piqini(mailla)
    implicit   none
#include "jeveux.h"
!
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8pi.h"
#include "asterfort/getvem.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexnom.h"
#include "asterfort/ordgmn.h"
#include "asterfort/piquag.h"
    character(len=8) :: mailla
!-----------------------------------------------------------------------
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
!     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
!     AUTEUR Y. WADIER
!
!     INITIALISE ET LANCE LE PROGRAMME PIQUAG
!
!-----------------------------------------------------------------------
!
!-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
!
!     EPT1  = EPAISSEUR PIQUAGE A LA BASE
!     DET1  = DIAMETRE EXTERIEUR PIQUAGE A LA BASE
!     D1    = HAUTEUR PIQUAGE A LA BASE
!     TYPSOU= TYPE DE LA SOUDURE (TYPE_1 OU TYPE_2)
!     H     = HAUTEUR SOUDURE
!     ALPHA = ANGLE ENTRE LES 2 INTERFACES
!     JEU   = JEU A LA BASE DE LA SOUDURE
!     DEC   = DIAMETRE EXTERIEUR CORPS A LA BASE
!     EPC   = EPAISSEUR CORPS A LA BASE
!     THETA = POSITION ANGULAIRE DU CENTRE DE LA FISSURE
!
!----------------------DONNEES FOURNIES PAR ASTER-----------------------
!
!     XMAX = LONGUEUR MAX SELON X, APRES ALLONGEMENT
!     YMAX = LONGUEUR MAX SELON Y
!     LMAX = LONGUEUR MAX SELON X
!     NT   = NOMBRE DE TRANCHES
!
!---------------CALCUL DES DONNEES DU PROGRAMME PIQUAGE-----------------
!
!     RIP = RAYON INTERIEUR DU PIQUAGE
!     REP = RAYON EXTERIEUR DU PIQUAGE
!     RIT = RAYON INTERIEUR DE LA TUYAUTERIE
!     RET = RAYON EXTERIEUR DE LA TUYAUTERIE
!
!     BET = ANGLE SOUDURE
!     ESO = EPAISSEUR SOUDURE
!     HSO = HAUTEUR SOUDURE
!
!     H2 = HAUTEUR  ZONE 2
!     H3 = HAUTEUR  ZONE 3
!     L3 = LONGUEUR ZONE 3
!     L4 = LONGUEUR ZONE 4
!     L5 = LONGUEUR ZONE 5
!     L6 = LONGUEUR ZONE 6
!
!     YMAX  = EPAISSEUR DE L'EQUERRE SUIVANT Y
!     TETAF = POSITION DU CENTRE DE LA FISSURE
!     NT    = NOMBRE DE TRANCHES
!-----------------------------------------------------------------------
!
    integer :: nt, n1, iret
    real(kind=8) :: epsi, rip, rep, rit, ret, bet, eso, hso, h2, h3, l3, l4, l5
    real(kind=8) :: l6, tetaf, rmc, d1, d2, ept1, det1, h, alpha, jeu, dec, epc
    real(kind=8) :: theta, xmax, ymax, lmax
    character(len=8) :: typsou
    character(len=8) :: typmai
    character(len=24) :: nogrno, grpnoe, nogrnp
    integer :: iarg
!     ------------------------------------------------------------------
!
!     ON ORDONNE LES NOEUDS SELON L'ORDRE DES MAILLES
!     CAR LORS DU CREA_GROUP L'ORDRE N'EST PAS CONSERVE
!
    call ordgmn(mailla, 'PFONDFIS')
!
    call getvem(mailla, 'GROUP_NO', 'EQUE_PIQUA', 'GROUP_NO', 1,&
                iarg, 1, nogrno, n1)
!
    call getvr8('EQUE_PIQUA', 'E_BASE', 1, iarg, 1,&
                ept1, n1)
    call getvr8('EQUE_PIQUA', 'DEXT_BASE', 1, iarg, 1,&
                det1, n1)
    call getvr8('EQUE_PIQUA', 'L_BASE', 1, iarg, 1,&
                d1, n1)
    call getvr8('EQUE_PIQUA', 'L_CHANF', 1, iarg, 1,&
                d2, n1)
    call getvtx('EQUE_PIQUA', 'TYPE', 1, iarg, 1,&
                typsou, n1)
    call getvr8('EQUE_PIQUA', 'H_SOUD', 1, iarg, 1,&
                h, n1)
    call getvr8('EQUE_PIQUA', 'ANGL_SOUD', 1, iarg, 1,&
                alpha, n1)
    call getvr8('EQUE_PIQUA', 'JEU_SOUD', 1, iarg, 1,&
                jeu, n1)
    call getvr8('EQUE_PIQUA', 'E_CORP', 1, iarg, 1,&
                epc, n1)
    call getvr8('EQUE_PIQUA', 'DEXT_CORP', 1, iarg, 1,&
                dec, n1)
    call getvr8('EQUE_PIQUA', 'AZIMUT   ', 1, iarg, 1,&
                theta, n1)
    call getvtx('EQUE_PIQUA', 'RAFF_MAIL', 1, iarg, 1,&
                typmai, n1)
    call getvr8('EQUE_PIQUA', 'X_MAX', 1, iarg, 1,&
                xmax, n1)
!
!
    rmc = ( dec - epc ) / 2.0d0
!
    lmax = r8pi() * rmc
    ymax = lmax - ( det1 / 2.0d0 ) + ept1
!
    if (typmai .eq. 'GROS') then
        nt = 20
    else
        nt = 24
    endif
!
    epsi = epc * 1.d-08
!
    rep = det1 / 2.0d0
    rip = rep - ept1
    ret = dec / 2.0d0
    rit = ret - epc
!
    bet = alpha * r8dgrd()
    hso = h
    h2 = ret + d1
    l3 = rep
    if (typsou .eq. 'TYPE_1') then
        eso = jeu + epc*tan(bet)
!
        h3 = ret + h
        l4 = rep + eso
    else if (typsou .eq. 'TYPE_2') then
        eso = jeu + ept1*tan(bet)
!
        h3 = ret + jeu + ept1*tan(bet)
        l4 = rep + hso
    endif
    l5 = 2.0d0 * l4
    l6 = lmax
!
    tetaf = theta * r8dgrd()
!
    call piquag(epsi, rip, rep, rit, ret,&
                bet, eso, hso, h2, h3,&
                l3, l4, l5, l6, tetaf,&
                xmax, ymax, lmax, nt, mailla,&
                nogrno, typsou)
!
    grpnoe = mailla//'.GROUPENO       '
    call jeexin(jexnom(grpnoe, 'P_FIS1'), iret)
    if (iret .ne. 0) then
        nogrnp = 'P_FIS1  '
        call piquag(epsi, rip, rep, rit, ret,&
                    bet, eso, hso, h2, h3,&
                    l3, l4, l5, l6, tetaf,&
                    xmax, ymax, lmax, nt, mailla,&
                    nogrnp, typsou)
    else
!
        call jeexin(jexnom(grpnoe, 'PI_FIS1'), iret)
        if (iret .ne. 0) then
            nogrnp = 'PI_FIS1  '
            call piquag(epsi, rip, rep, rit, ret,&
                        bet, eso, hso, h2, h3,&
                        l3, l4, l5, l6, tetaf,&
                        xmax, ymax, lmax, nt, mailla,&
                        nogrnp, typsou)
        endif
!
        call jeexin(jexnom(grpnoe, 'PS_FIS1'), iret)
        if (iret .ne. 0) then
            nogrnp = 'PS_FIS1  '
            call piquag(epsi, rip, rep, rit, ret,&
                        bet, eso, hso, h2, h3,&
                        l3, l4, l5, l6, tetaf,&
                        xmax, ymax, lmax, nt, mailla,&
                        nogrnp, typsou)
        endif
    endif
!
    call jeexin(jexnom(grpnoe, 'P_FIS2'), iret)
    if (iret .ne. 0) then
        nogrnp = 'P_FIS2  '
        call piquag(epsi, rip, rep, rit, ret,&
                    bet, eso, hso, h2, h3,&
                    l3, l4, l5, l6, tetaf,&
                    xmax, ymax, lmax, nt, mailla,&
                    nogrnp, typsou)
    else
        call jeexin(jexnom(grpnoe, 'PI_FIS2'), iret)
        if (iret .ne. 0) then
            nogrnp = 'PI_FIS2  '
            call piquag(epsi, rip, rep, rit, ret,&
                        bet, eso, hso, h2, h3,&
                        l3, l4, l5, l6, tetaf,&
                        xmax, ymax, lmax, nt, mailla,&
                        nogrnp, typsou)
        endif
!
        call jeexin(jexnom(grpnoe, 'PS_FIS2'), iret)
        if (iret .ne. 0) then
            nogrnp = 'PS_FIS2  '
            call piquag(epsi, rip, rep, rit, ret,&
                        bet, eso, hso, h2, h3,&
                        l3, l4, l5, l6, tetaf,&
                        xmax, ymax, lmax, nt, mailla,&
                        nogrnp, typsou)
        endif
    endif
!
end subroutine

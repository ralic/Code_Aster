subroutine posvar(compor, ndim, vari, nume)
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --- RECUPERATION DE L ADRESSE DE LA VARIABLE INTERNE NOMMEE
!   COMPOR  IN   K16 : COMPORTEMENT
!   NDIM    IN    I  : DIMENSION DU PROBLEME
!   VARI    IN   K16 : NOM DE LA VARIABLE INTERNE CHERCHE
!   NUME   OUT    I  : ADRESSE DE LA VARIABLE INTERNE
! ======================================================================
    implicit none
!
    include 'asterfort/nvithm.h'
    include 'asterfort/u2mesk.h'
    character(len=16) :: compor(*)
    character(len=24) :: vari
    character(len=24) :: valk(2)
    integer :: ndim, nume
    integer :: nvim, nvit, nvih, nvic, decal
    integer :: advime, advith, advihy, advico
    integer :: vihrho, vicphi, vicpvp, vicsat, vicpr1, vicpr2
    character(len=16) :: meca, thmc, ther, hydr
    call nvithm(compor, meca, thmc, ther, hydr,&
                nvim, nvit, nvih, nvic, advime,&
                advith, advihy, advico, vihrho, vicphi,&
                vicpvp, vicsat, vicpr1, vicpr2)
!
    if (( thmc .eq. 'LIQU_GAZ' ) .or. ( thmc .eq. 'LIQU_GAZ_ATM' )) then
        if (vari(1:6) .eq. 'DPORO') then
            nume=advico
            goto 9999
        else if (vari(1:6).eq.'DRHOLQ') then
            nume=advihy
            goto 9999
        else if (vari(1:6).eq.'DPVP') then
            nume=advico+1
            goto 9999
        else if (vari(1:6).eq.'SATLIQ') then
            nume=0
            call u2mesk('A', 'ALGORITH9_96', 1, thmc)
            goto 9999
        endif
        else if ( ( thmc .eq. 'LIQU_VAPE' ) .or. ( thmc .eq.&
    'LIQU_VAPE_GAZ' ) .or. ( thmc .eq. 'LIQU_AD_GAZ_VAPE') ) then
!
        if (vari(1:6) .eq. 'DPORO') then
            nume=advico
            goto 9999
        else if (vari(1:6).eq.'DRHOLQ') then
            nume=advihy
            goto 9999
        else if (vari(1:6).eq.'DPVP') then
            nume=advico+1
            goto 9999
        else if (vari(1:6).eq.'SATLIQ') then
            nume=advico+2
            goto 9999
        endif
    else if (thmc .eq. 'LIQU_AD_GAZ') then
!
!  DEUX VARIABLES INTERNES PRE1 ET PRE2 DE PLUS AJOUTEES
!  POUR VF ET UNIQUEMENT EN LIQU_AD_GAZ
!
        if (vari(1:6) .eq. 'DPORO') then
            nume=advico
            goto 9999
        else if (vari(1:6).eq.'DRHOLQ') then
            nume=advihy
            goto 9999
        else if (vari(1:6).eq.'DPVP') then
            nume=advico+1
            goto 9999
        else if (vari(1:6).eq.'SATLIQ') then
            nume=advico+2
            goto 9999
        else if (vari(1:6).eq.'PRE1  ') then
            nume=advico+3
            goto 9999
        else if (vari(1:6).eq.'PRE2  ') then
            nume=advico+4
            goto 9999
        endif
    else
        if (vari(1:6) .eq. 'DPORO') then
            nume=advico
            goto 9999
        else if (vari(1:6).eq.'DRHOLQ') then
            nume=advihy
            goto 9999
        else if (vari(1:6).eq.'DPVP') then
            call u2mesk('A', 'ALGORITH9_97', 1, thmc)
            nume=0
            goto 9999
        else if (vari(1:6).eq.'SATLIQ') then
            call u2mesk('A', 'ALGORITH9_96', 1, thmc)
            nume=0
            goto 9999
        endif
    endif
!-----LA LOI MECANIQUE EST CAM_CLAY
    if (meca(1:8) .eq. 'CAM_CLAY') then
!----- DEFORMATION VOLUMIQUE PLASTIQUE CUMULEE
        if (vari(1:3) .eq. 'PCR') then
            nume=advime
!----- INDICATEUR D ETAT
            goto 9999
        else if (vari(1:7).eq.'IND_ETA') then
            nume=advime+1
            goto 9999
!----- CONTRAINTE VOLUMIQUE
        else if (vari(1:5).eq.'SIGMO') then
            nume=advime+2
            goto 9999
!----- CONTRAINTE DEVIATORIQUE
        else if (vari(1:5).eq.'SIGDV') then
            nume=advime+3
            goto 9999
!----- DEFORMATION PLASTIQUE VOLUMIQUE
        else if (vari(1:5).eq.'EPSVO') then
            nume=advime+4
            goto 9999
!----- DEFORMATION PLASTIQUE EQUIVALENTE
        else if (vari(1:5).eq.'EPSEQ') then
            nume=advime+5
            goto 9999
!----- INDICE DES VIDES
        else if (vari(1:7).eq.'IND_VID') then
            nume=advime+6
            goto 9999
        else
            nume=-1
            goto 9999
        endif
!-----LA LOI MECANIQUE EST MAZARS
    else if (meca(1:6).eq.'MAZARS') then
!----- ENDOMMAGEMENT
        if (vari(1:1) .eq. 'D') then
            nume=advime
            goto 9999
!----- INDICATEUR D ENDOMMAGEMENT
        else if (vari(1:7).eq.'IND_END') then
            nume=advime+1
            goto 9999
!----- TEMPERATURE MAXIMALE AU POINT DE GAUSS
        else if (vari(1:8).eq.'TEMP_MAX') then
            nume=advime+2
            goto 9999
        else
            nume=-1
            goto 9999
        endif
!
!-----LA LOI MECANIQUE EST DRUCKER-PRAGER
!
    else if (meca(1:14).eq.'DRUCK_PRAGER') then
!----- DEFORMATION DEVIATOIRE PLASTIQUE CUMULEE
        if (vari(1:4) .eq. 'GAMP') then
            nume=advime
            goto 9999
!----- DEFORMATION VOLUMIQUE PLASTIQUE CUMULEE
        else if (vari(1:3).eq.'EVP') then
            nume=advime+1
            goto 9999
!----- INDICATEUR D ETAT
        else if (vari(1:7).eq.'IND_ETA') then
            nume=advime+2
            goto 9999
        else
            nume=-1
            goto 9999
        endif
!----- LOI MECANIQUE VISCOPLASTIQUE : VISC_DRUC_PRAG
    else if (meca(1:14).eq.'VISC_DRUC_PRAG') then
!----- VARIABLE D ECROUISSAGE VISCOPLASTIQUE
        if (vari(1:1) .eq. 'P') then
            nume=advime
            goto 9999
!----- INDICATEUR DE VISCOPLASTICITE
        else if (vari(1:4).eq.'PLAS') then
            nume=advime+1
            goto 9999
!----- POSITION DE P PAR RAPPORT AUX SEUILS
        else if (vari(1:3).eq.'POS') then
            nume=advime+2
            goto 9999
!----- NOMBRE D ITERATIONS LOCALES
        else if (vari(1:4).eq.'NBRE') then
            nume=advime+3
            goto 9999
        else
            nume=-1
            goto 9999
        endif
!-----LA LOI MECANIQUE EST ENDO_ISOT_BETON
    else if (meca(1:10).eq.'ENDO_ISOT_') then
!----- ENDOMMAGEMENT
        if (vari(1:1) .eq. 'D') then
            nume=advime
            goto 9999
!----- INDICATEUR D ENDOMMAGEMENT
        else if (vari(1:7).eq.'IND_END') then
            nume=advime+1
            goto 9999
        else
            nume=-1
            goto 9999
        endif
!-----LA LOI MECANIQUE EST BARCELONE
    else if (meca(1:9).eq.'BARCELONE') then
!----- PRESSION CRITIQUE
        if (vari(1:3) .eq. 'PCR') then
            nume=advime
            goto 9999
!----- INDICATEUR DE PLASTICITE MECANIQUE
        else if (vari(1:7).eq.'IND_ETA') then
            nume=advime+1
            goto 9999
!----- SEUIL HYDRIQUE
        else if (vari(1:9).eq.'SEUIL_HYD') then
            nume=advime+2
            goto 9999
!----- INDICATEUR D IRREVERSIBILITE HYDRIQUE
        else if (vari(1:7).eq.'IND_HYD') then
            nume=advime+3
            goto 9999
!----- PRESSION DE COHESION
        else if (vari(1:5).eq.'PCOHE') then
            nume=advime+4
            goto 9999
        else
            nume=-1
            goto 9999
        endif
!-----LA LOI MECANIQUE EST LAIGLE
    else if (meca(1:6).eq.'LAIGLE') then
!----- DEFORMATION DEVIATOIRE PLASTIQUE CUMULEE
        if (vari(1:4) .eq. 'GAMP') then
            nume=advime
            goto 9999
!----- DEFORMATION VOLUMIQUE PLASTIQUE CUMULEE
        else if (vari(1:3).eq.'EVP') then
            nume=advime+1
            goto 9999
!----- COMPORTEMENT DE LA ROCHE
        else if (vari(1:8).eq.'COMP_ROC') then
            nume=advime+2
            goto 9999
!----- INDICATEUR D ETAT
        else if (vari(1:7).eq.'IND_ETA') then
            nume=advime+3
            goto 9999
        else
            nume=-1
            goto 9999
        endif
!-----LA LOI MECANIQUE VISCOPLASTIQUE DU CIH : LETK
    else if (meca(1:6).eq.'LETK') then
!----- VARIABLE D ECROUISSAGE ELASTOPLASTIQUE
        if (vari(1:3) .eq. 'XIP') then
            nume=advime
            goto 9999
!----- DEFORMATION  DEVIATORIQUE PLASTIQUE
        else if (vari(1:6).eq.'GAMMAP') then
            nume=advime+1
            goto 9999
!----- VARIABLE D ECROUISSAGE VISCOPLASTIQUE
        else if (vari(1:3).eq.'XIV') then
            nume=advime+2
            goto 9999
!----- DEFORMATION  DEVIATORIQUE VISCOPLASTIQUE
        else if (vari(1:6).eq.'GAMMAV') then
            nume=advime+3
            goto 9999
!----- INDICATEUR DU DOMAINE DE CONTRACTANCE/DILATANCE
        else if (vari(1:5).eq.'VARV') then
            nume=advime+4
            goto 9999
!----- INDICATEUR DE LA VISCOSITE
        else if (vari(1:8).eq.'IND_VISC') then
            nume=advime+5
            goto 9999
!----- INDICATEUR DE LA PLASTICITE
        else if (vari(1:8).eq.'IND_PLAS') then
            nume=advime+6
            goto 9999
        else
            nume=-1
            goto 9999
        endif
!-----LA LOI MECANIQUE EST CJS
    else if (meca(1:3).eq.'CJS') then
        if (ndim .eq. 3) then
            decal=2
        else
            decal=0
        endif
!----- SEUIL ISOTROPE
        if (vari(1:9) .eq. 'SEUIL_ISO') then
            nume=advime
            goto 9999
!----- ANGLE DU SEUIL DEVIATOIRE
        else if (vari(1:7).eq.'ANG_DEV') then
            nume=advime+1
            goto 9999
!----- TENSEUR D ECROUISSAGE CINEMATIQUE
        else if (vari(1:3).eq.'X11') then
            nume=advime+2
            goto 9999
        else if (vari(1:3).eq.'X22') then
            nume=advime+3
            goto 9999
        else if (vari(1:3).eq.'X33') then
            nume=advime+4
            goto 9999
        else if (vari(1:3).eq.'X12') then
            nume=advime+5
            goto 9999
        else if (vari(1:3).eq.'X13') then
            if (ndim .eq. 3) then
                nume=advime+6
                goto 9999
            else
                call u2mesk('A', 'ALGORITH9_99', 1, vari)
                nume=0
            endif
        else if (vari(1:3).eq.'X23') then
            if (ndim .eq. 3) then
                nume=advime+7
                goto 9999
            else
                nume=0
                call u2mesk('A', 'ALGORITH9_99', 1, vari)
            endif
!----- DISTANCE NORMALISEE AU SEUIL DEVIATOIRE
        else if (vari(1:8).eq.'DIST_DEV') then
            nume=advime+decal+6
            goto 9999
!----- RAPPORT ENTRE LE SEUIL DEVIATOIRE
!----- ET LE SEUIL DEVIATORIQUE CRITIQUE
        else if (vari(1:12).eq.'DEV_SUR_CRIT') then
            nume=advime+decal+7
            goto 9999
!----- DISTANCE NORMALISEE AU SEUIL ISOTROPE
        else if (vari(1:8).eq.'DIST_ISO') then
            nume=advime+decal+8
            goto 9999
!----- NOMBRE D ITERATION INTERNE
        else if (vari(1:7).eq.'NB_ITER') then
            nume=advime+decal+9
            goto 9999
!----- VALEUR DU TEST LOCAL D ARRET DU PROCESSUS ITERATIF
        else if (vari(1:5).eq.'ARRET') then
            nume=advime+decal+10
            goto 9999
!----- NOMBRE DE REDECOUPAGE LOCAL DU PAS DE TEMPS
        else if (vari(1:7).eq.'NB_REDE') then
            nume=advime+decal+11
            goto 9999
!----- SIGNE DU PRODUIT CONTRACTE DE LA CONTRAINTE DEVIATORIQUE
!----- PAR LA DEFORMATION PLASTIQUE DEVIATORIQUE
        else if (vari(1:5).eq.'SIGNE') then
            nume=advime+decal+12
            goto 9999
!----- INDICATEUR D ETAT
        else if (vari(1:7).eq.'IND_ETA') then
            nume=advime+decal+13
            goto 9999
        else
            nume=-1
            goto 9999
        endif
    else
        nume=0
    endif
!-----LA LOI MECANIQUE EST HUJEUX
    if (meca(1:6) .eq. 'HUJEUX') then
!----- ECROUISSAGE DEVIATOIRE MECANISME 1
        if (vari(1:6) .eq. 'RDEV_1') then
            nume=advime
            goto 9999
!----- ECROUISSAGE DEVIATOIRE MECANISME 2
        else if (vari(1:6).eq.'RDEV_2') then
            nume=advime+1
            goto 9999
!----- ECROUISSAGE DEVIATOIRE MECANISME 3
        else if (vari(1:6).eq.'RDEV_3') then
            nume=advime+2
            goto 9999
!----- ECROUISSAGE ISOTROPE MECANISME 4
        else if (vari(1:4).eq.'RISO') then
            nume=advime+3
            goto 9999
!----- DEFORMATION PLASTIQUE VOLUMIQUE
        else if (vari(1:8).eq.'EPSIVPLA') then
            nume=advime+4
            goto 9999
!----- INDICATEUR D'ETAT MECANISME 1
        else if (vari(1:5).eq.'IND_1') then
            nume=advime+5
            goto 9999
!----- INDICATEUR D'ETAT MECANISME 2
        else if (vari(1:5).eq.'IND_2') then
            nume=advime+6
            goto 9999
!----- INDICATEUR D'ETAT MECANISME 3
        else if (vari(1:5).eq.'IND_3') then
            nume=advime+7
            goto 9999
!----- INDICATEUR D'ETAT MECANISME 4
        else if (vari(1:5).eq.'IND_4') then
            nume=advime+8
            goto 9999
        else
            nume=-1
            goto 9999
        endif
    else
        nume=0
    endif
9999  continue
    if (nume .eq. -1) then
        valk(1) = vari
        valk(2) = meca
        call u2mesk('A', 'ALGORITH10_1', 2, valk)
    endif
end subroutine

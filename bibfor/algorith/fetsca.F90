subroutine fetsca(nbi, vi, vo, scalin, infofe,&
                  nbi2, ifeti, ifm)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DE LA PHASE DE MISE A L'ECHELLE
!                         AU SENS FETI
!
!      IN    NBI: IN   : NOMBRE DE DDLS D'INTERFACE
!      IN     VI: VR8  : VECTEUR INPUT DE TAILLE NBI
!      OUT    VO: VR8  : VECTEUR OUTPUT DE TAILLE NBI
!      IN SCALIN: CH24 : PARAMETRE DE SCALING
!      IN NBI2 : IN   : NBRE DE LAGRANGE D'INTERFACE
!      IN IFETI: IN   : ADRESSE JEVEUX OBJET SDFETI.FETI
!      IN IFM  : IN   : UNITE D'IMPRESSION
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       28/01/04 (OB): CREATION.
!----------------------------------------------------------------------
! TOLE CRP_4
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/u2mess.h'
    include 'blas/dcopy.h'
    integer :: nbi, nbi2, ifeti, ifm
    real(kind=8) :: vi(nbi), vo(nbi)
    character(len=24) :: scalin, infofe
!
!
! DECLARATION VARIABLES LOCALES
    integer :: i, iaux, imult, nbddl, nbddlc, iauxj, j
    integer(kind=4) :: nbi4
    real(kind=8) :: rmult
!
! ROUTINE AVEC MOINS DE MONITORING, JEVEUX.. CAR APPELLEE SOUVENT
!
! MONITORING
    nbi4=nbi
    if (infofe(1:1) .eq. 'T') then
        if (scalin(1:4) .eq. 'SANS') then
            write(ifm,*)'<FETI/FETSCA> SANS SCALING'
        else
            write(ifm,*)'<FETI/FETSCA> SCALING PAR MULTIPLICITE'
        endif
    endif
!
    if (scalin(1:4) .eq. 'SANS') then
! ----------------------------------------------------------------------
! ----  PAS DE SCALING
! ----------------------------------------------------------------------
        call dcopy(nbi4, vi, 1, vo, 1)
!
    else if (scalin(1:4).eq.'MULT') then
! ----------------------------------------------------------------------
! ----  SCALING PAR MULTPLICITE
! ----------------------------------------------------------------------
!
        iaux=ifeti+1
! ---------------------------------------------------
! BOUCLE SUR LES NOEUDS D'INTERFACE
! ---------------------------------------------------
        do 20 i = 1, nbi2
!
! MULTIPLICITE DU IEME NOEUD D'INTERFACE
            imult=zi(iaux)
            if (imult .eq. 0) then
                call u2mess('F', 'ALGORITH3_72')
            else
! NOMBRE DE DDLS CUMULES AVANT LE IEME NOEUD D'INTERFACE (NBDDLC)
! NOMBRE DE DDLS DU IEME NOEUD D'INTERFACE (NBDDL)
                nbddlc=zi(iaux+1)
                if (i .eq. 1) then
                    nbddl=nbddlc
                else
                    nbddl=nbddlc-zi(iaux-3)
                endif
                nbddlc=nbddlc-nbddl
                rmult=1.d0/imult
                do 15 j = 1, nbddl
                    iauxj=nbddlc+j
                    vo(iauxj)=vi(iauxj)*rmult
15              continue
            endif
            iaux=iaux+4
20      continue
    else
        call assert(.false.)
    endif
! ---------------------------------------------------
! FIN BOUCLE SUR LES NOEUDS D'INTERFACE
! ---------------------------------------------------
!
! MONITORING
!      IF (INFOFE(4:4).EQ.'T') THEN
!        WRITE(IFM,*)'<FETI/FETSCA> INPUT I VI(I)'
!        DO 30 I=1,NBI
!          WRITE(IFM,*)I,'  ',VI(I)
!   30   CONTINUE
!        WRITE(IFM,*)'OUTPUT I VO(I)'
!        DO 31 I=1,NBI
!          WRITE(IFM,*)I,'  ',VO(I)
!   31   CONTINUE
!      ENDIF
end subroutine

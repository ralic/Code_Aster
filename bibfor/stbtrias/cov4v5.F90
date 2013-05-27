subroutine cov4v5(coddes, codgra)
    implicit none
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ============================================================
!     !                                                          !
!     !  AUTEURS:J.F.LAMAUDIERE                   DATE:18/04/91  !
!     !                                                          !
!     !                                                          !
!     ============================================================
!     !                                                          !
!     !  FONCTION:ASSURE LA CORRESPONDANCE ENTRE LE CODE DESCRI- !
!     !           PTEUR ( SUPERTAB I-DEAS 5.0 ) ET LE CODE GRA   !
!     !           PHIQUE ( SUPERTAB I-DEAS 4.0) POUR LES MAILLES !
!     !                                                          !
!     ============================================================
!     !                                                          !
!     !  SOUS PROGRAMMES APPELES : NEANT                         !
!     !                                                          !
!     !  SOUS PROGRAMME APPELANT : PRESUP                        !
!     !                                                          !
!     ============================================================
!     !                                                          !
!     !                   ***************                        !
!     !                   *  ARGUMENTS  *                        !
!     !                   ***************                        !
!     !                                                          !
!     !  ******************************************************  !
!     !  *   NOM    *  TYPE * MODE *MODIFIE*      ROLE        *  !
!     !  ******************************************************  !
!     !  *          *       *      *       *                  *  !
!     !  * CODDES   *INTEGER*ENTREE* NON   *CODE DESCRIPTEUR  *  !
!     !  *          *       *      *       * POUR UNE MAILLE  *  !
!     !  *          *       *      *       * (I-DEAS 5.0)     *  !
!     !  *          *       *      *       *                  *  !
!     !  * CODGRA   *INTEGER*SORTIE* NON   *CODE GRAPHIQUE    *  !
!     !  *          *       *      *       * DE LA MEME MAILLE*  !
!     !  *          *       *      *       *                  *  !
!     !  ******************************************************  !
!     !                                                          !
!     ============================================================
!
!  --> DECLARATION DES ARGUMENTS
    integer :: coddes, codgra
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (coddes .eq. 171) then
        codgra=1
        else if (coddes.eq.91 .or. coddes.eq.61 .or. coddes.eq.41 .or.&
    coddes.eq.51 .or. coddes.eq.74 .or. coddes.eq.81) then
        codgra=2
    else if (coddes.eq.11 .or. coddes.eq.21 .or. coddes.eq.23) then
        codgra=1
    else if (coddes.eq.22 .or. coddes.eq.24) then
        codgra=35
        else if (coddes.eq.92 .or. coddes.eq.62 .or. coddes.eq.42 .or.&
    coddes.eq.52 .or. coddes.eq.72 .or. coddes.eq.82) then
        codgra=3
        else if (coddes.eq.93 .or. coddes.eq.63 .or. coddes.eq.43 .or.&
    coddes.eq.53 .or. coddes.eq.73) then
        codgra=4
        else if (coddes.eq.94 .or. coddes.eq.64 .or. coddes.eq.44 .or.&
    coddes.eq.54 .or. coddes.eq.71 .or. coddes.eq.84) then
        codgra=5
        else if (coddes.eq.95 .or. coddes.eq.65 .or. coddes.eq.55 .or.&
    coddes.eq.45 .or. coddes.eq.75 .or. coddes.eq.85) then
        codgra=6
        else if (coddes.eq.96 .or. coddes.eq.66 .or. coddes.eq.46 .or.&
    coddes.eq.56 .or. coddes.eq.76) then
        codgra=7
    else if (coddes.eq.101) then
        codgra=8
    else if (coddes.eq.102) then
        codgra=9
    else if (coddes.eq.103) then
        codgra=10
    else if (coddes.eq.104) then
        codgra=11
    else if (coddes.eq.105) then
        codgra=12
    else if (coddes.eq.106) then
        codgra=13
    else if (coddes.eq.111) then
        codgra=14
    else if (coddes.eq.118) then
        codgra=15
    else if (coddes.eq.112) then
        codgra=16
    else if (coddes.eq.113) then
        codgra=17
    else if (coddes.eq.114) then
        codgra=18
    else if (coddes.eq.115) then
        codgra=19
    else if (coddes.eq.116) then
        codgra=20
    else if (coddes.eq.117) then
        codgra=21
    else if (coddes.eq.136 .or. coddes.eq.137) then
        codgra=29
    else if (coddes.eq.138 .or. coddes.eq.139) then
        codgra=30
    else if (coddes.eq.141) then
        codgra=31
    else if (coddes.eq.142) then
        codgra=32
    else if (coddes.eq.161) then
        codgra=33
    else if (coddes.eq.121) then
        codgra=34
    else if (coddes.eq.172) then
        codgra=35
    endif
!
end subroutine

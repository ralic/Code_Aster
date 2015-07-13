subroutine cgcrtb(table, option, lmelas, cas, typfis, nxpara,&
                  lmoda, nbpara, linopa, litypa)
!
    implicit none
!
#include "asterf_types.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/cgajpa.h"
#include "asterfort/assert.h"
    integer :: nbpara, nxpara
    aster_logical :: lmelas, lmoda
    character(len=*) :: litypa(nxpara), linopa(nxpara)
    character(len=8) :: table, typfis
    character(len=16) :: option, cas
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!     BUT : CREATION DE LA TABLE ISSUE DE CALC_G
!           ET AFFECTATION DES PARAMETRES
!
! ----------------------------------------------
!  IN :
!     TABLE : NOM DE LA TABLE
!     OPTION : OPTION DE CALCUL
!     LMELAS : .TRUE.  SI TYPE SD RESULTAT = MULT_ELAS
!              .FALSE. SINON
!     CAS    : '2D', '3D_LOCAL'  OU '3D_GLOBAL'
!     TYPFIS : TYPE D'OBJET POUR DECRIRE LE FOND DE FISSURE
!              'FONDFISS' OU 'FISSURE' OU 'THETA'
!     NXPARA : NOMBRE MAXI DE PARAMETRES DE LA TABLE
!     LMODA  : .TRUE.  SI TYPE SD RESULTAT = MODE_MECA
!              .FALSE. SINON
!
!  OUT :
!     NBPARA : NOMBRE DE PARAMETRES
!     linopa : NOMS DES PARAMETRES
!     litypa : TYPES DES PARAMETRES
! ----------------------------------------------
!
    integer :: i
    
    aster_logical :: debug
    
    nbpara = 0
    debug = .false.

!   --------------------
!   EXCLUSION DES OPTIONS A SUPPRIMER G_BILI(_GLOB) et G_MAX(_GLOB) et CALC_K_MAX
!   ---------------------  
    if ((option.ne.'G_BILI').and.(option.ne.'G_BILI_GLOB')&
        .and.(option.ne.'G_MAX').and.(option.ne.'G_MAX_GLOB')&
        .and.(option.ne.'CALC_K_MAX')) then
!-------------------------------------------------------------------------      
!   --------------------
!   1. IDENTIFICATEURS
!   --------------------
!   --------------------
!   1.1 FOND DE FISSURE
!   ---------------------      
        if ((typfis.ne.'THETA').and.(cas.ne.'3D_GLOBAL')) then
            call cgajpa('NUME_FOND', 'I', nbpara, linopa, litypa, nxpara)
        endif    
!   --------------------    
!   1.2 TEMPOREL/CHARGEMENT
!   ---------------------           
        if (lmelas) then
            call cgajpa('NUME_CAS', 'I', nbpara, linopa, litypa, nxpara)
            call cgajpa('NOM_CAS', 'K16', nbpara, linopa, litypa, nxpara)           
        elseif (lmoda) then
            call cgajpa('NUME_MODE', 'I', nbpara, linopa, litypa, nxpara)   
        else
            call cgajpa('NUME_ORDRE', 'I', nbpara, linopa, litypa, nxpara)
            call cgajpa('INST', 'R', nbpara, linopa, litypa, nxpara)    
        endif
!   --------------------    
!   1.3 POINT DU FOND DE FISSURE
!   ---------------------    
        if ((cas.ne.'3D_GLOBAL').and.(typfis.ne.'THETA')) then
            if (typfis.eq.'FONDFISS') then
                call cgajpa('NOEUD', 'K8', nbpara, linopa, litypa, nxpara)           
            endif
!            if (typfis.ne.'THETA') then
            call cgajpa('COOR_X', 'R', nbpara, linopa, litypa, nxpara)
            call cgajpa('COOR_Y', 'R', nbpara, linopa, litypa, nxpara)  
            if (cas.eq.'3D_LOCAL') then
                call cgajpa('COOR_Z', 'R', nbpara, linopa, litypa, nxpara)       
                call cgajpa('NUM_PT', 'I', nbpara, linopa, litypa, nxpara)
                call cgajpa('ABSC_CURV', 'R', nbpara, linopa, litypa, nxpara)
            endif
!            endif
        endif
!   --------------------
!   2. OPTIONS DE CALCUL
!   --------------------
!   --------------------
!   2.1 G COMMUN A TOUTES LES OPTIONS
!   ---------------------
        call cgajpa('G', 'R', nbpara, linopa, litypa, nxpara)
!   --------------------
!   2.2 CALC_K_G
!   ---------------------
        if (option.eq.'CALC_K_G') then
            call cgajpa('K1', 'R', nbpara, linopa, litypa, nxpara)
            call cgajpa('K2', 'R', nbpara, linopa, litypa, nxpara)
            call cgajpa('G_IRWIN', 'R', nbpara, linopa, litypa, nxpara)
            if (cas.eq.'3D_LOCAL') then
                call cgajpa('K3', 'R', nbpara, linopa, litypa, nxpara)
                call cgajpa('BETA', 'R', nbpara, linopa, litypa, nxpara)
            endif
        endif
!-------------------------------------------------------------------------
!   --------------------
!   2.3 OPTIONS A SUPPRIMER (G_BILI(_GLOB) et G_MAX(_GLOB) et CALC_K_MAX)
!   ---------------------
    elseif (option.eq.'CALC_K_MAX') then
        nbpara = 15
        linopa(1) = 'NUME_FOND'
        litypa(1) = 'I'
        if (lmelas) then
            linopa(2) = 'NUME_CAS'
            litypa(2) = 'I'
            linopa(3) = 'NOM_CAS'
            litypa(3) = 'K16'
        else
            linopa(2) = 'NUME_ORDRE'
            litypa(2) = 'I'
            linopa(3) = 'INST'
            litypa(3) = 'R'
        endif
        if (typfis.eq.'FONDFISS') then
            linopa(4) = 'NOEUD'
            litypa(4) = 'K8'
        endif
        linopa(5) = 'COOR_X'
        litypa(5) = 'R'
        linopa(6) = 'COOR_Y'
        litypa(6) = 'R'
        linopa(7) = 'COOR_Z'
        litypa(7) = 'R'
        linopa(8) = 'NUM_PT'
        litypa(8) = 'I'
        linopa(9) = 'ABSC_CURV'
        litypa(9) = 'R'
        linopa(10) = 'K1'
        litypa(10) = 'R'
        linopa(11) = 'K2'
        litypa(11) = 'R'
        linopa(12) = 'K3'
        litypa(12) = 'R'
        linopa(13) = 'G'
        litypa(13) = 'R'
        linopa(14) = 'BETA'
        litypa(14) = 'R'
        linopa(15) = 'G_IRWIN'
        litypa(15) = 'R'
    elseif ((option.eq.'G_BILI').or.(option.eq.'G_MAX')) then
        nbpara = 6
        if (lmelas) then
            linopa(1) = 'NOM_CAS'
            litypa(1) = 'K16'
        else
            linopa(1) = 'INST'
            litypa(1) = 'R'
        endif
        linopa(2) = 'NUME_CMP_I'
        litypa(2) = 'I'
        linopa(3) = 'NUME_CMP_J'
        litypa(3) = 'I'
        linopa(4) = 'NOEUD'
        litypa(4) = 'K8'
        linopa(5) = 'ABSC_CURV'
        litypa(5) = 'R'
        linopa(6) = 'G_BILI_LOCAL'
        litypa(6) = 'R'
    elseif ((option.eq.'G_BILI_GLOB').or.(option.eq.'G_MAX_GLOB')) then
        nbpara = 3
        linopa(1) = 'NUME_CMP_I'
        litypa(1) = 'I'
        linopa(2) = 'NUME_CMP_J'
        litypa(2) = 'I'
        linopa(3) = 'G_BILIN'
        litypa(3) = 'R'
    endif
      
!   --------------------
!   3. CREATION DE LA TABLE
!   --------------------
    call tbcrsd(table, 'G')
    call tbajpa(table, nbpara, linopa, litypa)

!   --------
!   4. DEBUG
!   --------
    if (debug) then
        write(6,*)'OPTION = ', option
        write(6,*)'NOMBRE DE PARAMETRES DE LA TABLE = ', nbpara
        write(6,*)'NO_PARA, NOM_PARA, TYP_PARA'
        do 10 i = 1, nbpara
            write(6,*)i, linopa(i), litypa(i)
        10 enddo
    endif

end subroutine

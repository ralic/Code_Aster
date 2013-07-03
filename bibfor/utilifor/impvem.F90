subroutine impvem(ifi)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
    implicit none
#include "asterc/ismaem.h"
#include "asterc/isnnem.h"
#include "asterc/ispbem.h"
#include "asterc/lbisem.h"
#include "asterc/loc8em.h"
#include "asterc/lofiem.h"
#include "asterc/loisem.h"
#include "asterc/lolsem.h"
#include "asterc/lor8em.h"
#include "asterc/mofiem.h"
#include "asterc/ncisem.h"
#include "asterc/ncr8em.h"
#include "asterc/r8baem.h"
#include "asterc/r8depi.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8gaem.h"
#include "asterc/r8maem.h"
#include "asterc/r8miem.h"
#include "asterc/r8nnem.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterc/r8rddg.h"
#include "asterc/r8vide.h"
#include "asterc/rmarem.h"
#include "asterc/rmaxem.h"
#include "asterc/rminem.h"
#include "asterc/rmirem.h"
    integer :: ifi
! ----------------------------------------------------------------------
!
!     ECRITURE DES RESULTATS DES FONCTIONS ENVIMA
!        ASSOCIES A LA MACHINE COURANTE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: cft1is, cft2is, cftis
    character(len=34) :: cft1r8, cft2r8, cft3r8, cftr8
!
!
!-----------------------------------------------------------------------
    integer :: i, ntest
!-----------------------------------------------------------------------
    parameter    ( ntest = 32 )
    character(len=38) :: label(ntest)
!
    data ( label(i), i=1,7) /&
     &   'XXXXXX  XXXXXX   XXXXXXX    ',&
     &   'LBISEM  ENTIER   INTEGER    ',&
     &   'XXXXXX  XXXXXX   XXXXXXX    ',&
     &   'LOLSEM  LOGIQUE  LOGICAL    ',&
     &   'LOISEM  ENTIER   INTEGER    ',&
     &   'LOR8EM  REEL     REAL*8     ',&
     &   'LOC8EM  COMPLEXE COMPLEX*16 ' /
!
    data ( label(i), i=8,12) /&
     &   'XXXXXX  ',&
     &   'XXXXXX  ',&
     &   'XXXXXX  ',&
     &   'LOFIEM  EN OCTETS      ',&
     &   'MOFIEM  EN OCTETS      '   /
!
    data ( label (i), i=13,14 ) /&
     &   'ISMAEM  ENTIER   INTEGER    ',&
     &   'ISNNEM  ENTIER   INTEGER    '  /
!
    data ( label(i), i=15,25 )     /&
     &   'R8BAEM  BASE NUMERATION    REAL*8    ',&
     &   'R8PREM  PRECISION RELATIVE REAL*8    ',&
     &   'R8MAEM  MAXIMAL            REAL*8    ',&
     &   'R8MIEM  MINIMAL            REAL*8    ',&
     &   'R8GAEM  GAMME              REAL*8    ',&
     &   'R8NNEM  NOT A NUMBER       REAL*8    ',&
     &   'R8VIDE  VIDE               REAL*8    ',&
     &   'RMIREM  B**-T'                        ,&
     &   'RMAREM  B**(1-T)'                     ,&
     &   'RMINEM  B**(EMIN-1)'                  ,&
     &   'RMAXEM  B**EMAX(1-B**(-T))'           /
!
    data ( label(i), i=26,29 )     /&
     &   'R8PI    REEL  REAL*8     ',&
     &   'R8DEPI  REEL  REAL*8     ',&
     &   'R8DGRD  REEL  REAL*8     ',&
     &   'R8RDDG  REEL  REAL*8     '  /
!    ------------------------------------------------------
!       FORMATS AUTOMATIQUES
!  123456789012345678901234
    cft1is = '(1X,A,IX,2X,Z16)'
    cft2is = '(1X,A,IXX,2X,Z16)'
    if (ncisem() .lt. 9) then
        write ( cft1is( 8: 8) , '(I1)' ) ncisem() + 1
        cftis = cft1is
    else
        write ( cft2is( 8: 9) , '(I2)' ) ncisem() + 1
        cftis = cft2is
    endif
!  12345678901234567890123456789012345
    cft1r8 = '(1X,A,1PDX.X,  2X,Z16)'
    cft2r8 = '(1X,A,1PDXX.X, 2X,Z16)'
    cft3r8 = '(1X,A,1PDXX.XX,2X,Z16)'
!  12345678901234567890123456789012345
    if (ncr8em()+8 .lt. 10) then
        write ( cft1r8(10:10) , '(I1)' ) ncr8em()+8
        write ( cft1r8(12:12) , '(I1)' ) ncr8em()-1
        cftr8 = cft1r8
    else
        write ( cft2r8(10:11) , '(I2)' ) ncr8em()+8
        write ( cft3r8(10:11) , '(I2)' ) ncr8em()+8
        if (ncr8em()-1 .lt. 10) then
            write ( cft2r8(13:13) , '(I1)' ) ncr8em()-1
            cftr8 = cft2r8
        else
            write ( cft3r8(13:14) , '(I2)' ) ncr8em()-1
            cftr8 = cft3r8
        endif
    endif
!
!    ------------------------------------------------------
!
    write ( ifi , '(/,(A))' )&
     &   '-------------------------------------------------' ,&
     &   '---- ENVIMA VERSION 97 MULTI MACHINES    --------' ,&
     &   '-------------------------------------------------'
!
    write ( ifi , '(/,A,/)' )&
     &   ' ----- MACHINE UTILISEE'
    write ( ifi , '(/,A,/)' )&
     &   ' ----- LONGUEUR EN BITS'
    write ( ifi , '((1X,A,I3,2X))' ) label( 2) , lbisem()
!
    write ( ifi , '(/,A,/)' )&
     &   ' ----- LONGUEUR EN OCTETS'
    write ( ifi , '((1X,A,I3))' )&
     &   label( 4) , lolsem() ,&
     &   label( 5) , loisem() ,&
     &   label( 6) , lor8em() ,&
     &   label( 7) , loc8em()
!
    write ( ifi , '(/,A,/)' )&
     &   ' ----- NOMBRE DE CHIFFRES SIGNIFICATIFS'
    write ( ifi , '((1X,A,I3))' )&
     &   label( 8) , ncisem() ,&
     &   label( 9) , ncr8em()
!
    write ( ifi , '(/,A,/)' )&
     &   ' ----- LONGUEUR ET TAILLE DE FICHIER'
    write ( ifi , '((1X,A,I10))' )  label(11) , lofiem()
    write ( ifi , '((1X,A,I10,2X,I10,1X,A))' )&
     &   label(12) , mofiem()
!
    write ( ifi , '(/,A,/)' )&
     &   ' ----- ENTIER STANDARD'
    write ( ifi , cftis )&
     &   label(13) , ismaem() , ismaem() ,&
     &   label(14) , isnnem() , isnnem()
!
    write ( ifi , '(/,A,/)' )&
     &   ' ----- REAL*8'
!
    write ( ifi , '(/,A,'' '',A/)' )&
     &   ' FORMAT D''IMPRESSION DES FLOTTANTS',cftr8
!
    write ( ifi , cftr8 )&
     &   label(15) , r8baem() , r8baem() ,&
     &   label(16) , r8prem() , r8prem() ,&
     &   label(17) , r8maem() , r8maem() ,&
     &   label(18) , r8miem() , r8miem() ,&
     &   label(19) , r8gaem() , r8gaem() ,&
     &   label(20) , r8nnem() , r8nnem() ,&
     &   label(21) , r8vide() , r8vide() ,&
     &   label(22) , rmirem() , rmirem() ,&
     &   label(23) , rmarem() , rmarem() ,&
     &   label(24) , rminem() , rminem() ,&
     &   label(25) , rmaxem() , rmaxem()
!
    write ( ifi , '(/,A,/,A,/)' )&
     &   ' ----- POIDS DES BITS 1 A LBIS' ,&
     &   ' ISPBEM  BITS NUMEROTES DE DROITE A GAUCHE'
    do 10 i = 1, lbisem()-1
        write ( ifi , '(1X,I3,2X,I20)' ) i , ispbem(i)
10  end do
! ----------------------------------------------------------------------
    write ( ifi , '(/,A,/)' )&
     &   ' ----- VALEURS PARTICULIERES  PI, DEPI, ...    '
    write ( ifi , cftr8 )&
     &   label(26) , r8pi()   , r8pi()   ,&
     &   label(27) , r8depi() , r8depi() ,&
     &   label(28) , r8dgrd() , r8dgrd() ,&
     &   label(29) , r8rddg() , r8rddg()
!
    write ( ifi , '(/,A)' )&
     &   '-------------------------------------------------' ,&
     &   '---- FIN TEST ENVIMA MULTI MACHINES -------------' ,&
     &   '   OK     ' ,&
     &   '-------------------------------------------------' ,&
     &   ' '
end subroutine

subroutine titred(niv, nomcon, nomcha, nbtitr)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: niv, nomcon, nomcha
    integer :: nbtitr
!     ------------------------------------------------------------------
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
!     GENERATION D'UN TITRE OU D'UN SOUS-TITRE PAR DEFAUT
!     ------------------------------------------------------------------
! IN  NIV    : K1  : NIVEAU  'T': TITRE, 'S': SOUS-TITRE
! IN  NOMCON : K8  : NOM DU RESULTAT
! IN  NOMCHA : K19 : NOM DU CONCEPT (UTILISATEUR OU INTERNE)
! OUT NBTITR : I   : NOMBRE DE LIGNE DU TITRE GENERE
!     ------------------------------------------------------------------
!
!
    character(len=8) :: typesd
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ichoix, ierd, ldon, llon, mxdef
    integer :: mxlig
!-----------------------------------------------------------------------
    parameter            (mxdef=9 , mxlig= 8 )
    character(len=72) :: defaut(mxlig, mxdef)
    integer :: londef(mxlig, mxdef), nblig(mxdef)
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!     1/ --- TITRE PAR DEFAUT ---
!               1         2         3         4         5         6
!      12345678901234567890123456789012345678901234567890123456789012345
    data nblig(1),(defaut(i,1),londef(i,1),i=1,3)/&
     &       3,&
     &'ASTER &VERSION',14,&
     &' CONCEPT &RESULTAT CALCULE LE &DATE A &HEURE',44,&
     &' DE TYPE &TYPE',14/
!     ------------------------------------------------------------------
!
!     2/ --- SOUS-TITRES PAR DEFAUT D'UN CHAM_NO
!               1         2         3         4         5         6
!      12345678901234567890123456789012345678901234567890123456789012345
    data nblig(2),(defaut(i,2),londef(i,2),i=1,1)/&
     &       1,&
     &'CHAMP AUX NOEUDS',16/
!     ------------------------------------------------------------------
!
!     3/ --- SOUS-TITRES PAR DEFAUT D'UN CHAM_ELEM
!               1         2         3         4         5         6
!      12345678901234567890123456789012345678901234567890123456789012345
    data nblig(3),(defaut(i,3),londef(i,3),i=1,2)/&
     &       2,&
     &'CHAMP PAR ELEMENT',17,&
     &' &LOC(12345678)',15/
!     ------------------------------------------------------------------
!
!     4/ --- SOUS-TITRES PAR DEFAUT D'UN CHAM_NO D'UN RESULTAT
!               1         2         3         4         5         6
!      12345678901234567890123456789012345678901234567890123456789012345
    data nblig(4),(defaut(i,4),londef(i,4),i=1,7)/&
     &       7,&
     &'CHAMP AUX NOEUDS DE',19,&
     &' NOM SYMBOLIQUE ',16,&
     &' &NOM_SYMB(''1234567890123456789'',''1234567890123456789'')',55,&
     &' &RL',4,&
     &'NUMERO D''ORDRE: ',16,&
     &'&NUME_ORDRE(''1234567890123456789'',''1234567890123456789'')',56,&
     &' &ACCES(''1234567890123456789'',''1234567890123456789'')',52/
!     ------------------------------------------------------------------
!
!     5/ --- SOUS-TITRES PAR DEFAUT D'UN CHAM_ELEM D'UN RESULTAT
!               1         2         3         4         5         6
!      12345678901234567890123456789012345678901234567890123456789012345
    data nblig(5),(defaut(i,5),londef(i,5),i=1,8)/&
     &       8,&
     &'CHAMP PAR ELEMENT',17,&
     &' &LOC(''1234567890123456789'') DE',31,&
     &' NOM SYMBOLIQUE ',16,&
     &' &NOM_SYMB(''1234567890123456789'',''1234567890123456789'')',55,&
     &' &RL',4,&
     &'NUMERO D''ORDRE: ',16,&
     &'&NUME_ORDRE(''1234567890123456789'',''1234567890123456789'')',56,&
     &' &ACCES(''1234567890123456789'',''1234567890123456789'')',52/
!     ------------------------------------------------------------------
!
!     6/ SOUS-TITRES PAR DEFAUT D'UN VECTEUR GENERALISE D'UN RESULTAT
!               1         2         3         4         5         6
!      12345678901234567890123456789012345678901234567890123456789012345
    data nblig(6),(defaut(i,6),londef(i,6),i=1,7)/&
     &       7,&
     &'VECTEUR GENERALISE DE',21,&
     &' NOM SYMBOLIQUE ',16,&
     &' &NOM_SYMB(''1234567890123456789'',''1234567890123456789'')',55,&
     &' &RL',4,&
     &'NUMERO D''ORDRE: ',16,&
     &'&NUME_ORDRE(''1234567890123456789'',''1234567890123456789'')',56,&
     &' &ACCES(''1234567890123456789'',''1234567890123456789'')',52/
!     ------------------------------------------------------------------
!
!     7/ --- SOUS-TITRES PAR DEFAUT D'UN VECTEUR GENERALISE
!               1         2         3         4         5         6
!      12345678901234567890123456789012345678901234567890123456789012345
    data nblig(7),(defaut(i,7),londef(i,7),i=1,1)/&
     &       1,&
     &'VECTEUR GENERALISE',18/
!     ------------------------------------------------------------------
!
!     8/ --- SOUS-TITRES PAR DEFAUT D'UNE CARTE D'UN RESULTAT
!               1         2         3         4         5         6
!      12345678901234567890123456789012345678901234567890123456789012345
    data nblig(8),(defaut(i,8),londef(i,8),i=1,7)/&
     &       7,&
     &'CARTE DE',8,&
     &' NOM SYMBOLIQUE ',16,&
     &' &NOM_SYMB(''1234567890123456789'',''1234567890123456789'')',55,&
     &' &RL',4,&
     &'NUMERO D''ORDRE: ',16,&
     &'&NUME_ORDRE(''1234567890123456789'',''1234567890123456789'')',56,&
     &' &ACCES(''1234567890123456789'',''1234567890123456789'')',52/
!     ------------------------------------------------------------------
!
!     2/ --- SOUS-TITRES PAR DEFAUT D'UNE CARTE
!               1         2         3         4         5         6
!      12345678901234567890123456789012345678901234567890123456789012345
    data nblig(9),(defaut(i,9),londef(i,9),i=1,1)/&
     &       1,&
     &'CARTE',5/
!     ------------------------------------------------------------------
!
!
!     --- CHOIX D'UN TITRE ---
    call jemarq()
    if (niv .eq. 'T') then
        ichoix = 1
    else
        call dismoi('TYPE_RESU', nomcon, 'RESULTAT', repk=typesd, arret='C',&
                    ier=ierd)
        if (ierd .ne. 0) then
            call utmess('F', 'UTILITAI4_99', sk=typesd)
        else if (typesd .eq. 'CHAMP') then
            call dismoi('TYPE_CHAMP', nomcon, 'CHAMP', repk=typesd, arret='C',&
                        ier=ierd)
            if (typesd(1:4) .eq. 'NOEU') then
                ichoix = 2
            else if (typesd(1:2) .eq. 'EL') then
                ichoix = 3
                defaut(2,ichoix)(07:14) = nomcon
            else if (typesd(1:4) .eq. 'VGEN') then
                ichoix = 7
            else if (typesd(1:4) .eq. 'CART') then
                ichoix = 9
            else
                call utmess('F', 'UTILITAI4_99', sk=typesd)
            endif
        else
            call dismoi('TYPE_CHAMP', nomcha, 'CHAMP', repk=typesd, arret='C',&
                        ier=ierd)
!CCC +                                                             IERD)
            if (typesd(1:4) .eq. 'NOEU') then
                ichoix = 4
                defaut(3,ichoix)(13:31) = nomcon
                defaut(3,ichoix)(35:53) = nomcha
                defaut(6,ichoix)(14:32) = nomcon
                defaut(6,ichoix)(36:54) = nomcha
                defaut(7,ichoix)(10:28) = nomcon
                defaut(7,ichoix)(32:50) = nomcha
            else if (typesd(1:2) .eq. 'EL') then
                ichoix = 5
                defaut(2,ichoix)(08:26) = nomcha
                defaut(4,ichoix)(13:31) = nomcon
                defaut(4,ichoix)(35:53) = nomcha
                defaut(7,ichoix)(14:32) = nomcon
                defaut(7,ichoix)(36:54) = nomcha
                defaut(8,ichoix)(10:28) = nomcon
                defaut(8,ichoix)(32:50) = nomcha
            else if (typesd(1:4) .eq. 'VGEN') then
                ichoix = 6
                defaut(3,ichoix)(13:31) = nomcon
                defaut(3,ichoix)(35:53) = nomcha
                defaut(6,ichoix)(14:32) = nomcon
                defaut(6,ichoix)(36:54) = nomcha
                defaut(7,ichoix)(10:28) = nomcon
                defaut(7,ichoix)(32:50) = nomcha
            else if (typesd(1:4) .eq. 'CART') then
                ichoix = 8
                defaut(3,ichoix)(13:31) = nomcon
                defaut(3,ichoix)(35:53) = nomcha
                defaut(6,ichoix)(14:32) = nomcon
                defaut(6,ichoix)(36:54) = nomcha
                defaut(7,ichoix)(10:28) = nomcon
                defaut(7,ichoix)(32:50) = nomcha
            else
                call utmess('F', 'UTILITAI4_99', sk=typesd)
            endif
        endif
    endif
!
!     --- REMPLISSAGE DU TITRE CHOISI ---
    nbtitr = nblig(ichoix)
    call wkvect('&&TITRE .TAMPON.ENTREE', 'V V K80', nbtitr, ldon)
    call wkvect('&&TITRE .LONGUEUR     ', 'V V I  ', nbtitr, llon)
!-DEL WRITE(6,*)  ' DEBUT DU DEFAUT NUMERO ',ICHOIX,'---------------- '
    do 10 i = 1, nbtitr
        zk80(ldon+i-1) = defaut(i,ichoix)
        zi(llon+i-1) = londef(i,ichoix)
 10 end do
!_DEL WRITE(6,*)  ' FIN DU DEFAUT ----------------------------------- '
!
    call jedema()
end subroutine

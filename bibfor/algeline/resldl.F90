subroutine resldl(solveu, nommat, vcine, nsecm, rsolu,&
                  csolu, prepos)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/csmbgg.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mcconl.h"
#include "asterfort/mrconl.h"
#include "asterfort/mtdscr.h"
#include "asterfort/rldlg3.h"
#include "asterfort/u2mess.h"
    character(len=*) :: nommat, vcine
    integer :: nsecm
    real(kind=8) :: rsolu(*)
    complex(kind=8) :: csolu(*)
    logical :: prepos
!
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
!-----------------------------------------------------------------------
! BUT : RESOUDRE UN SYSTEME LINEAIRE D'EQUATIONS (REEL OU COMPLEXE)
!       SOLVEUR = 'LDLT' OU 'MULT_FRONT'
!-----------------------------------------------------------------------
! IN/JXIN  K19 SOLVEU : SD_SOLVEUR
! IN/JXIN  K19 NOMMAT : MATR_ASSE PREMIER MEMBRE DU SYSTEME LINEAIRE
! IN/JXIN  K*  VCINE  : CHAMP ASSOCIE AUX CHARGES CINEMATIQUES (OU ' ')
! IN       I   NSECM  :  N : NOMBRE DE SECONDS MEMBRES
! IN/OUT   R   RSOLU(*,NSECM)  :
!        EN ENTREE : VECTEUR DE REELS CONTENANT LES SECONDS MEMBRES
!        EN SORTIE : VECTEUR DE REELS CONTENANT LES SOLUTIONS
! IN/OUT   C   CSOLU(*,NSECM)  : IDEM RSOLU POUR LES COMPLEXES.
! IN      LOG  PREPOS : SI .TRUE. ON FAIT LES PRE ET POSTTRAITEMENTS DE
!           MISE A L'ECHELLE DU RHS ET DE LA SOLUTION (MRCONL) ET DE LA
!           PRISE EN COMPTE DES AFFE_CHAR_CINE (CSMBGG).
!           SI .FALSE. ON NE LES FAIT PAS (PAR EXEMPLE EN MODAL).
!-----------------------------------------------------------------------
!
!
    character(len=19) :: nomma2
    character(len=8) :: type
    character(len=16) :: metres
    character(len=19) :: vci19, solveu
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
    integer :: k, kdeb, idvalc, lmat, neq, nimpo, islvk
!     ------------------------------------------------------------------
!
    call jemarq()
    vci19=vcine
    nomma2=nommat
!
    call jeveuo(solveu//'.SLVK', 'L', islvk)
    metres=zk24(islvk)
!
    call mtdscr(nomma2)
    call jeveuo(nomma2(1:19)//'.&INT', 'E', lmat)
    if (lmat .eq. 0) call u2mess('F', 'ALGELINE3_40')
!
    neq=zi(lmat+2)
    nimpo=zi(lmat+7)
    if (vci19 .eq. ' ') then
! --- SI ON NE FAIT PAS LES PREPOS, ON NE SE PREOCCUPE PAS DES
!     AFFE_CHAR_CINE. DONC C'EST NORMAL QUE L'INFO SOIT INCOHERENTE
!     A CE NIVEAU
        if ((nimpo.ne.0) .and. prepos) call u2mess('F', 'ALGELINE3_41')
        idvalc=0
    else
        call jeveuo(vci19//'.VALE', 'L', idvalc)
        call jelira(vci19//'.VALE', 'TYPE', cval=type)
        if (((type.eq.'R').and.(zi(lmat+3).ne.1)) .or.&
            ((type.eq.'C') .and.(zi(lmat+3).ne.2))) then
            call u2mess('F', 'ALGELINE3_42')
        endif
    endif
!
    if (zi(lmat+3) .eq. 1) then
        type='R'
    else if (zi(lmat+3).eq.2) then
        type='C'
    else
        ASSERT(.false.)
    endif
!
!
!
!
    if (type .eq. 'R') then
!     ----------------------------------------
        if (prepos) then
!         MISE A L'ECHELLE DES LAGRANGES DANS LE SECOND MEMBRE
            call mrconl('MULT', lmat, 0, 'R', rsolu,&
                        nsecm)
            if (idvalc .ne. 0) then
                do 10,k=1,nsecm
                kdeb=(k-1)*neq+1
                call csmbgg(lmat, rsolu(kdeb), zr(idvalc), cbid, cbid,&
                            'R')
10              continue
            endif
        endif
        call rldlg3(metres, lmat, rsolu, cbid, nsecm)
        if (prepos) then
!         MISE A L'ECHELLE DES LAGRANGES DANS LA SOLUTION
            call mrconl('MULT', lmat, 0, 'R', rsolu,&
                        nsecm)
        endif
!
!
    else if (type.eq.'C') then
!     ----------------------------------------
        if (prepos) then
!         MISE A L'ECHELLE DES LAGRANGES DANS LE SECOND MEMBRE
            call mcconl('MULT', lmat, 0, 'C', csolu,&
                        nsecm)
            if (idvalc .ne. 0) then
                do 20,k=1,nsecm
                kdeb=(k-1)*neq+1
                call csmbgg(lmat, rbid, rbid, csolu(kdeb), zc(idvalc),&
                            'C')
20              continue
            endif
        endif
        call rldlg3(metres, lmat, rbid, csolu, nsecm)
        if (prepos) then
!         MISE A L'ECHELLE DES LAGRANGES DANS LA SOLUTION
            call mcconl('MULT', lmat, 0, 'C', csolu,&
                        nsecm)
        endif
    endif
!
!
    call jedema()
end subroutine

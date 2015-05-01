subroutine op0128()
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 13/07/90
!-----------------------------------------------------------------------
!  BUT: ASSEMBLER UNE MATRICE ISSUE D'UN MODELE GENERALISE
!
!     CONCEPT CREE: MATR_ASSE_GENE
!
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/asgeel.h"
#include "asterfort/assgcy.h"
#include "asterfort/assgen.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jeexin.h"
    integer :: iret,jdesc,n1,n2
    character(len=8) :: nomres, numeg
    character(len=9) :: method
    character(len=11) :: option
    character(len=14) :: nugene
    character(len=16) :: nomcon, nomope
    character(len=19) :: nomr19
!-----------------------------------------------------------------------
    integer :: ibid, iopt
!-----------------------------------------------------------------------
    call infmaj()
!
    call getres(nomres, nomcon, nomope)
!
!-------------------RECUPERATION CONCEPTS AMONT-------------------------
!
    call getvid(' ', 'NUME_DDL_GENE', scal=numeg, nbret=ibid)
    nugene=numeg
!
!-------------------------RECUPERATION DE L'OPTION----------------------
!
    call getvtx(' ', 'OPTION', scal=option, nbret=ibid)
!
!---------------------------------ASSEMBLAGE----------------------------
!
    call getvtx(' ', 'METHODE', scal=method, nbret=iopt)
!
!-- on teste si les objets pour l'elimination existent
    call jeexin(nugene//'.ELIM.BASE', iret)
!
    if (method .eq. 'CLASSIQUE') then
        if (iret .gt. 0) then
            call asgeel(nomres, option, nugene)
        else
            call assgen(nomres, option, nugene)
        endif
    else
        if (iret .gt. 0) then
            call asgeel(nomres, option, nugene)
        else
            call assgcy(nomres, nugene)
        endif
    endif


!   -- on corrige l'objet .DESC :
!   ------------------------------------------------------------------------------
    nomr19=nomres
    call jelira(nomr19//'.CONL','LONMAX',n1)
    call jelira(jexnum(nomr19//'.VALM',1),'LONMAX',n2)
    call jeveuo(nomr19//'.DESC','E',jdesc)
    zi(jdesc)=2
    zi(jdesc+1)=n1
    if (n2.eq.n1) then
        zi(jdesc+2)=1
    elseif (n2.eq.n1*(n1+1)/2) then
        zi(jdesc+2)=2
    else
        zi(jdesc+2)=3
    endif


end subroutine

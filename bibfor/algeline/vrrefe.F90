subroutine vrrefe(objet1, objet2, ier)
    implicit none
#include "jeveux.h"
#include "asterfort/idensd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=*) :: objet1, objet2
    integer :: ier
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
!     ------------------------------------------------------------------
!     VERIFICATION QUE DEUX CONCEPTD ONT MEME DOMAINE DE DEFINITION
!         ==> COMPARAISON DES "REFE"
!     LES CONCEPTS DOIVENT ETRE DE TYPE MATR_ASSE_GD, CHAM_NO
!     OU CHAM_ELEM
!     ------------------------------------------------------------------
! IN  OBJET1  : CH*19 : NOM DU 1-ER CONCEPT
! IN  OBJET2  : CH*19 : NOM DU 2-ND CONCEPT
! OUT IER     : IS   : CODE RETOUR
!                = 0 PAS D'ERREUR
!                > 0 NOMBRE DE DESCRIPTEURS DIFFERENTS
!     ------------------------------------------------------------------
!
    logical(kind=1) :: ok
!
    integer :: ival1, ival2
    character(len=19) :: nom1, nom2
    character(len=24) :: refe1, refe2
    logical(kind=1) :: refa, celk, lgene
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: irefe1, irefe2, iret
!-----------------------------------------------------------------------
    call jemarq()
!
    ier = 0
    nom1 = objet1
    nom2 = objet2
!
!  SI OBJET1 ET OBJET2 SONT DES CHAM_NO : ON COMPARE LEUR .REFE
!  SI OBJET1 ET OBJET2 SONT DES MATR_ASSE : ON COMPARE LEUR .REFA
    refa=.false.
    celk=.false.
    refe1 = nom1//'.REFE'
    call jeexin(refe1, iret)
    if (iret .gt. 0) then
        refe2 = nom2//'.REFE'
    else
        refe1 = nom1//'.REFA'
        call jeexin(refe1, iret)
        if (iret .gt. 0) then
            refe2 = nom2//'.REFA'
            refa=.true.
        else
            refe1 = nom1//'.CELK'
            call jeexin(refe1, iret)
            if (iret .gt. 0) then
                refe2 = nom2//'.CELK'
                celk=.true.
            else
                call utmess('F', 'ALGELINE3_90')
            endif
        endif
    endif
!
!
!     --- RECUPERATION DES LONGUEURS DES TABLEAUX DE REFERENCE ---
    call jelira(refe1, 'LONMAX', ival1)
    call jelira(refe2, 'LONMAX', ival2)
    if (ival1 .ne. ival2) ier = ier + abs(ival1-ival2)
!
!     --- RECUPERATION DES TABLEAUX D'INFORMATIONS DE REFERENCE ---
    call jeveuo(refe1, 'L', irefe1)
    call jeveuo(refe2, 'L', irefe2)
!
!     --- CONTROLE DES REFERENCES ---
!
    if (refa) then
        lgene=zk24(irefe1-1+10).eq.'GENE'
        if (.not.lgene) then
!         -- CAS DES MATR_ASSE :
            if (zk24(irefe1-1+1) .ne. zk24(irefe2-1+1)) ier=ier+1
            if (zk24(irefe1-1+2) .ne. zk24(irefe2-1+2)) ier=ier+1
        else
!         -- CAS DES MATR_ASSE_GENE :
            if (zk24(irefe1-1+2) .ne. zk24(irefe2-1+2)) ier=ier+1
        endif
!
    else if (celk) then
!       -- cas des cham_elem :
!       POUR LES CHAM_ELEM ON NE VERIFIE PAS L OPTION
!       CAR QUELQUES OPTIONS METALLURGIQUES PRODUISENT
!       DES CHAMPS QUE L ON SOUHAITE COMBINER
!       ON VERIFIE DONC LE LIGREL ET LE TYPE DE CHAMP
        if (zk24(irefe1) .ne. zk24(irefe2)) ier=ier+1
        call jeveuo(nom1//'.CELD', 'L', irefe1)
        call jeveuo(nom2//'.CELD', 'L', irefe2)
        if (zi(irefe1) .ne. zi(irefe2)) ier=ier+1
!
    else
!       -- cas des cham_no :
        if (zk24(irefe1) .ne. zk24(irefe2)) ier=ier+1
        ok=idensd('PROF_CHNO',zk24(irefe1+1),zk24(irefe2+1))
        if (.not.ok) ier=ier+1
    endif
!
    call jedema()
end subroutine

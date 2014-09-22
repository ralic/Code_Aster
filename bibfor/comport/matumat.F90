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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    subroutine matumat(fami, kpg, ksp, imate, ifm, niv, idbg, nprops, props)
!     but: coef materiau pour interface umat
!       in   fami    famille de point de gauss (rigi,mass,...)
!            kpg,ksp numero du (sous)point de gauss
!            imate   adresse du materiau code
!       out  nprops  nb coef
!            props   coef materiau
! ======================================================================
! aslint: disable=W1504,W0104
    implicit none
#include "asterc/r8nnem.h"
#include "asterfort/infniv.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
    integer          :: imate, kpg, ksp, nprops, idbg, ifm, niv
    integer          :: codrel(197), nbcoef, i
    character(len=*) :: fami
    character(len=16) :: nomres(197)
    real(kind=8)     :: props(*), propl(197)
    data nomres/'C1','C2','C3','C4','C5','C6','C7','C8','C9','C10',&
     &'C11','C12','C13','C14','C15','C16','C17','C18','C19','C20','C21',&
     &'C22','C23','C24','C25','C26','C27','C28','C29','C30','C31','C32',&
     &'C33','C34','C35','C36','C37','C38','C39','C40','C41','C42','C43',&
     &'C44','C45','C46','C47','C48','C49','C50','C51','C52','C53','C54',&
     &'C55','C56','C57','C58','C59','C60','C61','C62','C63','C64','C65',&
     &'C66','C67','C68','C69','C70','C71','C72','C73','C74','C75','C76',&
     &'C77','C78','C79','C80','C81','C82','C83','C84','C85','C86','C87',&
     &'C88','C89','C90','C91','C92','C93','C94','C95','C96','C97','C98',&
     &'C99','C100','C101','C102','C103','C104','C105','C106','C107',&
     &'C108','C109','C110','C111','C112','C113','C114','C115','C116',&
     &'C117','C118','C119','C120','C121','C122','C123','C124','C125',&
     &'C126','C127','C128','C129','C130','C131','C132','C133','C134',&
     &'C135','C136','C137','C138','C139','C140','C141','C142','C143',&
     &'C144','C145','C146','C147','C148','C149','C150','C151','C152',&
     &'C153','C154','C155','C156','C157','C158','C159','C160','C161',&
     &'C162','C163','C164','C165','C166','C167','C168','C169','C170',&
     &'C171','C172','C173','C174','C175','C176','C177','C178','C179',&
     &'C180','C181','C182','C183','C184','C185','C186','C187','C188',&
     &'C189','C190','C191','C192','C193','C194','C195','C196','C197'/
!
!     LECTURE DES PROPRIETES MATERIAU (MOT-CLE UMAT DE DEFI_MATERIAU)
    call r8inir(nprops, r8nnem(), props, 1)
!
!     LECTURE DU PREMIER PARAMETRE NB, FACULTATIF
    call rcvalb(fami, kpg, ksp, '+', imate, ' ', 'UMAT', 0, ' ', [0.d0], &
    &           1, 'NB_VALE', propl(1), codrel, 0)
    if (codrel(1) .eq. 0) then
        nbcoef=nint(propl(1))
    else
        nbcoef=nprops
    endif
!     lecture des autres parametres
    call rcvalb(fami, kpg, ksp, '+', imate, ' ', 'UMAT', 0, ' ', [0.d0], &
    &           nbcoef, nomres, propl, codrel, 0)
!     COMPTAGE DU NOMBRE DE PROPRIETES
!     CODREL(I)=0 SI LE PARAMETRE EXISTE, 1 SINON
    if ((niv.ge.2) .and. (idbg.eq.1)) then
        write(ifm,*)' '
        write(ifm,*)'COEFFICIENTS MATERIAU'
    endif
    nprops=0
    do 20 i = 1, nbcoef
        if (codrel(i) .eq. 0) then
            nprops=nprops+1
            props(nprops)=propl(i)
            if ((niv.ge.2) .and. (idbg.eq.1)) then
                write(ifm,*) nomres(i),props(nprops)
            endif
        endif
 20 continue
    end

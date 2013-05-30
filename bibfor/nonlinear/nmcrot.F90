subroutine nmcrot(result, sdobse)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/exisd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/ltcrsd.h'
    include 'asterfort/ltnotb.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: result
    character(len=19) :: sdobse
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - OBSERVATION)
!
! CREATION TABLE OBSERVATION
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM SD RESULTAT
! IN  SDOBSE : NOM DE LA SD POUR OBSERVATION
!
! ----------------------------------------------------------------------
!
    integer :: nbpara
    parameter   (nbpara=16)
    character(len=8) :: typara(nbpara)
    character(len=16) :: nopara(nbpara)
!
    character(len=24) :: obstab
    integer :: jobst
    integer :: iret
    character(len=19) :: nomtab
!
    data nopara/'NOM_OBSERVATION','TYPE_OBJET'  ,'NOM_SD' ,&
     &            'NUME_REUSE'     ,'NUME_OBSE'   ,'INST'   ,&
     &            'NOM_CHAM'       ,'EVAL_CHAM'   ,'NOM_CMP',&
     &            'EVAL_CMP'       ,'NOEUD'       ,'MAILLE' ,&
     &            'EVAL_ELGA'      ,'POINT'       ,'SOUS_POINT',&
     &            'VALE'           /
    data typara/'K80','K16','K24',&
     &            'I'  ,'I'  ,'R'  ,&
     &            'K16','K8' ,'K8' ,&
     &            'K8' ,'K8' ,'K8' ,&
     &            'K8' ,'I'  ,'I'  ,&
     &            'R' /
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- SD NOM TABLE
!
    obstab = sdobse(1:14)//'     .TABL'
    call wkvect(obstab, 'V V K24', 1, jobst)
!
! --- CREATION DE LA LISTE DE TABLES SI ELLE N'EXISTE PAS
!
    call jeexin(result//'           .LTNT', iret)
    if (iret .eq. 0) call ltcrsd(result, 'G')
!
! --- RECUPERATION DE LA TABLE CORRESPONDANT AUX OBSERVATIONS
!
    nomtab = ' '
    call ltnotb(result, 'OBSERVATION', nomtab)
!
! --- CREATION DE LA TABLE CORRESPONDANT AUX OBSERVATIONS
!
    call exisd('TABLE', nomtab, iret)
    if (iret .eq. 0) then
        call tbcrsd(nomtab, 'G')
        call tbajpa(nomtab, nbpara, nopara, typara)
    endif
!
! --- SAUVEGARDE NOM DU TABLEAU
!
    zk24(jobst) = nomtab
!
    call jedema()
end subroutine

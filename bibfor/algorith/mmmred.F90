subroutine mmmred(ndimg, lctfc, champ, champr, ndd1)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/cnsred.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: ndimg
    character(len=19) :: champ, champr
    logical :: lctfc
    integer :: ndd1
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - POST-TRAITEMENT)
!
! REDUCTION DU CHAMP SUR LES DDL
!
! ----------------------------------------------------------------------
!
!
! IN  NDIMG  : DIMENSION DE L'ESPACE
! IN  LCTFC  : .TRUE. SI FROTTEMENT
! IN  CHAMP  : CHAM_NO A REDUIRE
! OUT CHAMPR : CHAM_NO_S REDUIT DE L'INCREMENT DE DEPLACEMENT CUMULE
! OUT NDD1   : NOMBRE DE DDL/NOEUD
!
!
!
!
    character(len=8) :: licmp4(4), licmp6(6)
    character(len=19) :: champs
! ----------------------------------------------------------------------
    data licmp4&
     &   / 'DX'     ,'DY'      ,&
     &     'LAGS_C' ,'LAGS_F1' /
    data licmp6&
     &   / 'DX'     ,'DY'      ,'DZ'      ,&
     &     'LAGS_C' ,'LAGS_F1' ,'LAGS_F2' /
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- TRANSFORMATION DU CHAM_NO EN CHAM_NO_S
!
    champs = '&&MMMRED.CHAMPS'
    call cnocns(champ, 'V', champs)
!
! --- REDUCTION DU CHAM_NO_S DES DDL EN UN CHAM_NO_S DES LAGRANGES
! --- DE CONTACT/FROTTEMENT
!
    if (ndimg .eq. 3) then
        if (lctfc) then
            ndd1 = 6
        else
            ndd1 = 4
        endif
        call cnsred(champs, 0, 0, ndd1, licmp6,&
                    'V', champr)
    else if (ndimg.eq.2) then
        if (lctfc) then
            ndd1 = 4
        else
            ndd1 = 3
        endif
        call cnsred(champs, 0, 0, ndd1, licmp4,&
                    'V', champr)
    else
        call assert(.false.)
    endif
!
    call detrsd('CHAMP', champs)
!
    call jedema()
end subroutine

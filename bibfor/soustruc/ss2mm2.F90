subroutine ss2mm2(mo, vecel, nomcas)
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
! INSPI  SS2MME
    implicit none
!
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    character(len=8) :: mo, nomcas
    character(len=19) :: vecel
! ----------------------------------------------------------------------
!     BUT: TRAITER LE MOT-CLEF CAS_CHARGE DE LA COMMANDE
!          MACR_ELEM_STAT (POUR LES MACR_ELEM DU NIVEAU INFERIEUR)
!
!
!     IN:     MO : NOM DU MODELE
!          VECEL : NOM DU VECT_ELEM
!          NOMCAS: NOM DU CAS_CHARGE
!
!     OUT: VECEL EST  ENRICHI (EVENTUELLEMENT) DE L'OBJET .LISTE_CHAR
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: ma, kbid, nosma, nomacr
!
!
!
!
!-----------------------------------------------------------------------
    integer :: i, ialsch, iamacr, iarefr, iasssa, ibid, ierd
    integer :: iret, nbsma, nbssa
!-----------------------------------------------------------------------
    call jemarq()
    call dismoi('F', 'NOM_MAILLA', mo, 'MODELE', ibid,&
                ma, ierd)
    call dismoi('F', 'NB_SS_ACTI', mo, 'MODELE', nbssa,&
                kbid, ierd)
    call dismoi('F', 'NB_SM_MAILLA', mo, 'MODELE', nbsma,&
                kbid, ierd)
!
    if (nbssa .eq. 0) goto 9999
!
    call jeveuo(mo//'.MODELE    .SSSA', 'L', iasssa)
    call jeveuo(ma//'.NOMACR', 'L', iamacr)
!
    call jeveuo(vecel//'.RERR', 'E', iarefr)
    zk24(iarefr-1+3)(1:3)='OUI'
!
    call jecrec(vecel//'.RELC', 'V V I', 'NO', 'CONTIG', 'CONSTANT',&
                1)
    call jeecra(vecel//'.RELC', 'LONMAX', nbsma, kbid)
    call jecroc(jexnom(vecel//'.RELC', nomcas))
    call jeveuo(jexnom(vecel//'.RELC', nomcas), 'E', ialsch)
!
!
!     -- REMPLISSAGE DE .RELC:
!     ------------------------------
!
!     -- ON VERIFIE QUE LES VECTEURS ELEMENTAIRES SONT CALCULES:
!     ----------------------------------------------------------
    do 3, i=1,nbsma
    if (zi(iasssa-1+i) .eq. 0) goto 3
    call jenuno(jexnum(ma//'.SUPMAIL', i), nosma)
    nomacr= zk8(iamacr-1+i)
    call jeexin(jexnom(nomacr//'.LICA', nomcas), iret)
    if (iret .gt. 0) then
        zi(ialsch-1+i)=1
    else
        zi(ialsch-1+i)=0
    endif
    3 end do
!
!
!
!
!
9999  continue
    call jedema()
end subroutine

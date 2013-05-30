subroutine xpocox(nbmac, ima, inmtot, nbcmpc, jresd1,&
                  jresv1, jresl1, jresd2, jresv2, jresl2)
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: nbmac, ima, inmtot, jresd1, jresv1, jresl1, nbcmpc
    integer :: jresd2, jresv2, jresl2
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
! person_in_charge: samuel.geniaut at edf.fr
!
!   ECRITURE DU COMPORTEMENT SUR LE RESU X-FEM POUR LES MAILLES X-FEM
!
!   IN
!     IMA    : NUMÉRO DE LA MAILLE PARENT
!     INMTOT : NB MAILLES X-FEM DEJA CREES
!     NBCMPC : NB DE CMP DU COMPORTEMENT
!     JRESD1 : ADRESSE DU .CESD DU CHAM_ELEM_S DE COMPORTEMENT ENTREE
!     JRESV1 : ADRESSE DU .CESV DU CHAM_ELEM_S DE COMPORTEMENT ENTREE
!     JRESL1 : ADRESSE DU .CESL DU CHAM_ELEM_S DE COMPORTEMENT ENTREE
!
!   OUT
!     JRESD1 : ADRESSE DU .CESD DU CHAM_ELEM_S DE COMPORTEMENT SORTIE
!     JRESV1 : ADRESSE DU .CESV DU CHAM_ELEM_S DE COMPORTEMENT SORTIE
!     JRESL1 : ADRESSE DU .CESL DU CHAM_ELEM_S DE COMPORTEMENT SORTIE
!
!
!
!
!
    integer :: icmp, iadr1, iadr2, ima2
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     NUMERO DE LA NOUVELLE MAILLE
    ima2 = nbmac + inmtot
!
!     COMPORTEMENT
    do 310 icmp = 1, nbcmpc
!
        call cesexi('C', jresd1, jresl1, ima, 1,&
                    1, icmp, iadr1)
        call cesexi('C', jresd2, jresl2, ima2, 1,&
                    1, icmp, iadr2)
!
        if (iadr1 .gt. 0) then
            call assert(iadr2.lt.0)
            zk16(jresv2-1-iadr2) = zk16(jresv1-1+iadr1)
            zl(jresl2-1-iadr2) = .true.
        endif
!
310  end do
!
    call jedema()
end subroutine

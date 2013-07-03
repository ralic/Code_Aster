subroutine cuimp1(deficu, resocu, ifm)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/cudisd.h"
#include "asterfort/cudisi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: deficu
    character(len=24) :: resocu
    integer :: ifm
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATER (DEBUG)
!
! IMPRESSION DES JEUX
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICU : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCU : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
!
!
!
!
    integer :: iliac, iliai, actif
    integer :: nbliai, nbliac
    character(len=24) :: liac, apjeu, nomnoe, nomcmp
    integer :: jliac, japjeu, jnomno, jnomcm
    character(len=8) :: cmp, noe
    real(kind=8) :: jeu
    character(len=15) :: chaiac
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD LIAISON_UNILATER
!
    liac = resocu(1:14)//'.LIAC'
    apjeu = resocu(1:14)//'.APJEU'
    nomnoe = resocu(1:14)//'.NOMNOE'
    nomcmp = resocu(1:14)//'.NOMCMP'
!
    call jeveuo(liac, 'L', jliac)
    call jeveuo(apjeu, 'L', japjeu)
    call jeveuo(nomnoe, 'L', jnomno)
    call jeveuo(nomcmp, 'L', jnomcm)
!
! --- INFORMATIONS SUR LE NOMBRE DE LIAISONS
!
    nbliai = cudisi(deficu,'NNOCU' )
    nbliac = cudisd(resocu,'NBLIAC')
!
! --- BOUCLE SUR LES LIAISONS
!
    do 500 iliai = 1, nbliai
!
! --- NOEUD ET COMPOSANTE DE LA LIAISON
!
        cmp = zk8(jnomcm-1+iliai)
        noe = zk8(jnomno-1+iliai)
!
! --- JEU
!
        jeu = zr(japjeu-1+iliai)
!
! --- ACTIF OU NON ?
!
        actif = 0
!
        do 10 iliac = 1, nbliac
            if (zi(jliac-1+iliac) .eq. iliai) then
                actif = 1
            endif
10      continue
!
! --- IMPRESSION
!
        if (actif .eq. 1) then
            chaiac = ' ACTIVE (JEU : '
        else
            chaiac = ' LIBRE  (JEU : '
        endif
        write (ifm,1000) iliai,'(',noe,' - ',cmp,') :',&
     &                   chaiac,jeu,')'
500  end do
!
    1000 format (' <LIA_UNIL> <> LIAISON ',i5,a1,a8,a3,a8,a4,a15,e10.3,a1)
!
    call jedema()
!
end subroutine

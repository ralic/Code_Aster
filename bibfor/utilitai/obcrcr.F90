subroutine obcrcr(nomstr, nbparb, nbpari, nbparr, nbpark,&
                  nbparo, parab, parai, parar, parak,&
                  parao, typeo)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=24) :: nomstr
    integer :: nbparb, nbpari, nbparr, nbpark, nbparo
    character(len=24) :: parab(nbparb)
    character(len=24) :: parai(nbpari)
    character(len=24) :: parar(nbparr)
    character(len=24) :: parak(nbpark)
    character(len=24) :: parao(nbparo)
    character(len=24) :: typeo(nbparo)
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT)
!
! CREATION
!
! ----------------------------------------------------------------------
!
!
! IN  NOMSTR : NOM DU STRUCT
! IN  NBPARB : NOMBRE DE PARAMETRES DE TYPE BOOLEEN
! IN  NBPARI : NOMBRE DE PARAMETRES DE TYPE ENTIER
! IN  NBPARR : NOMBRE DE PARAMETRES DE TYPE REEL
! IN  NBPARK : NOMBRE DE PARAMETRES DE TYPE CHAINE
! IN  NBPARO : NOMBRE DE PARAMETRES DE TYPE OBJET
! IN  PARAB  : LISTE DES PARAMETRES DE TYPE BOOLEEN
! IN  PARAI  : LISTE DES PARAMETRES DE TYPE ENTIER
! IN  PARAR  : LISTE DES PARAMETRES DE TYPE REEL
! IN  PARAK  : LISTE DES PARAMETRES DE TYPE CHAINE
! IN  PARAO  : LISTE DES PARAMETRES DE TYPE OBJET
! IN  TYPEO  : LISTE DES TYPES D'OBJETS
!
! ----------------------------------------------------------------------
!
    character(len=24) :: sdvalb, sdvali, sdvalr, sdvalk, sdvalo, sdtypo
    integer :: jsvalb, jsvali, jsvalr, jsvalk, jsvalo, jstypo
    character(len=24) :: sdpara
    integer :: jspara
    integer :: jdecal, nbpara, ipara
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOM DES OBJETS JEVEUX
!
    sdpara = nomstr(1:19)//'.PARA'
    sdvalb = nomstr(1:19)//'.VALB'
    sdvali = nomstr(1:19)//'.VALI'
    sdvalr = nomstr(1:19)//'.VALE'
    sdvalk = nomstr(1:19)//'.VALK'
    sdvalo = nomstr(1:19)//'.VALO'
    sdtypo = nomstr(1:19)//'.TYPO'
!
! --- CREATION SEGMENTS DE VALEUR
!
    if (nbparb .ne. 0) call wkvect(sdvalb, 'V V I  ', nbparb, jsvalb)
    if (nbpari .ne. 0) call wkvect(sdvali, 'V V I  ', nbpari, jsvali)
    if (nbparr .ne. 0) call wkvect(sdvalr, 'V V R  ', nbparr, jsvalr)
    if (nbpark .ne. 0) call wkvect(sdvalk, 'V V K24', nbpark, jsvalk)
    if (nbparo .ne. 0) call wkvect(sdvalo, 'V V K24', nbparo, jsvalo)
    if (nbparo .ne. 0) call wkvect(sdtypo, 'V V K24', nbparo, jstypo)
!
! --- CREATION PARAMETRES
!
    nbpara = nbparb+nbpari+nbparr+nbpark+nbparo
    ASSERT(nbpara.gt.0)
    call wkvect(sdpara, 'V V K24', nbpara, jspara)
!
! --- NOM DES PARAMETRES
!
    jdecal = 0
    do 10 ipara = 1, nbparb
        zk24(jspara-1+ipara+jdecal) = parab(ipara)
10  end do
    jdecal = nbparb
    do 20 ipara = 1, nbpari
        zk24(jspara-1+ipara+jdecal) = parai(ipara)
20  end do
    jdecal = jdecal+nbpari
    do 30 ipara = 1, nbparr
        zk24(jspara-1+ipara+jdecal) = parar(ipara)
30  end do
    jdecal = jdecal+nbparr
    do 40 ipara = 1, nbpark
        zk24(jspara-1+ipara+jdecal) = parak(ipara)
40  end do
    jdecal = jdecal+nbpark
    do 50 ipara = 1, nbparo
        zk24(jspara-1+ipara+jdecal) = parao(ipara)
        zk24(jstypo-1+ipara) = typeo(ipara)
50  end do
!
    call jedema()
end subroutine

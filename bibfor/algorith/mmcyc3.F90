subroutine mmcyc3(resoco, iptc, nompt, indco, indfr,&
                  resnew)
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
! aslint: disable=
    implicit     none
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterc/r8rddg.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/normev.h'
    include 'blas/ddot.h'
    character(len=24) :: resoco
    integer :: iptc
    character(len=16) :: nompt
    integer :: indco, indfr
    real(kind=8) :: resnew(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE)
!
! DETECTION DU CYCLE DE TYPE GLISSANT AVANT/ARRIERE
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  INDCO  : STATUT DE CONTACT
! IN  INDFR  : STATUT DE FROTTEMENT
! IN  NOMPT  : NOM DU POINT DE CONTACT
! IN  IPTC   : NUMERO DE LA LIAISON DE CONTACT
! IN  RESNEW : MULTIPLICATEUR AUGMENTE DU FROTTEMENT (NORMALISE)
!
!
!
!
    character(len=24) :: cyclis, cycnbr, cyctyp, cycpoi, cycgli
    integer :: jcylis, jcynbr, jcytyp, jcypoi, jcygli
    integer :: ccycle
    logical :: detect
    real(kind=8) :: resold(3)
    real(kind=8) :: noor1, noor2
    real(kind=8) :: angle, prosca, val, angtol
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    detect = .false.
    angtol = 1.d0
!
! --- ACCES OBJETS
!
    cyclis = resoco(1:14)//'.CYCLIS'
    cycnbr = resoco(1:14)//'.CYCNBR'
    cyctyp = resoco(1:14)//'.CYCTYP'
    cycpoi = resoco(1:14)//'.CYCPOI'
    call jeveuo(cyclis, 'E', jcylis)
    call jeveuo(cycnbr, 'E', jcynbr)
    call jeveuo(cyctyp, 'E', jcytyp)
    call jeveuo(cycpoi, 'E', jcypoi)
    cycgli = resoco(1:14)//'.CYCGLI'
    call jeveuo(cycgli, 'E', jcygli)
!
! --- ETAT PRECEDENT
!
    ccycle = zi(jcylis-1+4*(iptc-1)+3)
!
! --- PAS DE CONTACT: FIN DU CYCLE DIRECTEMENT
!
    if (indco .eq. 0) then
        zi(jcylis-1+4*(iptc-1)+3) = 0
        zi(jcynbr-1+4*(iptc-1)+3) = 0
        zi(jcytyp-1+4*(iptc-1)+3) = 0
        zk16(jcypoi-1+4*(iptc-1)+3) = ' '
        zr(jcygli-1+3*(iptc-1)+1) = 0.d0
        zr(jcygli-1+3*(iptc-1)+2) = 0.d0
        zr(jcygli-1+3*(iptc-1)+3) = 0.d0
        goto 99
    endif
!
! --- ADHERENT: FIN DE CYCLE DIRECTEMENT
!
    if (indfr .eq. 0) then
        zi(jcylis-1+4*(iptc-1)+3) = 0
        zi(jcynbr-1+4*(iptc-1)+3) = 0
        zi(jcytyp-1+4*(iptc-1)+3) = 0
        zk16(jcypoi-1+4*(iptc-1)+3) = ' '
        zr(jcygli-1+3*(iptc-1)+1) = 0.d0
        zr(jcygli-1+3*(iptc-1)+2) = 0.d0
        zr(jcygli-1+3*(iptc-1)+3) = 0.d0
        goto 99
    endif
!
! --- ETAT PRECEDENT ADHERENT: FIN DE CYCLE DIRECTEMENT
!
    if (ccycle .eq. 1) then
        zi(jcylis-1+4*(iptc-1)+3) = indfr
        zi(jcynbr-1+4*(iptc-1)+3) = 0
        zi(jcytyp-1+4*(iptc-1)+3) = 0
        zk16(jcypoi-1+4*(iptc-1)+3) = ' '
        zr(jcygli-1+3*(iptc-1)+1) = 0.d0
        zr(jcygli-1+3*(iptc-1)+2) = 0.d0
        zr(jcygli-1+3*(iptc-1)+3) = 0.d0
        goto 99
    endif
!
! --- MISE A JOUR
!
    call assert(indfr.eq.1)
    call assert(ccycle.eq.0)
!
! --- DETECTION D'UN CYCLE
!
    resold(1) = zr(jcygli-1+3*(iptc-1)+1)
    resold(2) = zr(jcygli-1+3*(iptc-1)+2)
    resold(3) = zr(jcygli-1+3*(iptc-1)+3)
    call normev(resold, noor1)
    call normev(resnew, noor2)
!
! --- CALCUL DE L'ANGLE
!
    prosca = ddot(3,resnew,1,resold,1)
    detect = .false.
    if (abs(noor1*noor2) .gt. r8prem()) then
        val = prosca/(noor1*noor2)
        if (val .gt. 1.d0) val = 1.d0
        if (val .lt. -1.d0) val = -1.d0
        angle = acos(val)
        angle = angle*r8rddg()
        if (abs(angle-180.d0) .le. angtol) then
            detect = .true.
        endif
    endif
!
! --- SAUVEGARDE DU CYCLE
!
    zr(jcygli-1+3*(iptc-1)+1) = resnew(1)
    zr(jcygli-1+3*(iptc-1)+2) = resnew(2)
    zr(jcygli-1+3*(iptc-1)+3) = resnew(3)
    zi(jcylis-1+4*(iptc-1)+3) = 0
!
! --- OK
!
    if (detect) then
        zi(jcytyp-1+4*(iptc-1)+3) = 1
        zk16(jcypoi-1+4*(iptc-1)+3) = nompt
    endif
!
99  continue
!
    call jedema()
end subroutine

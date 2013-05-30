subroutine te0496(option, nomte)
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/jevech.h'
    include 'asterfort/tecach.h'
    character(len=16) :: option, nomte
    include 'jeveux.h'
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     POUR TOUS LES TYPE_ELEM CALCUL DU NOMBRE :
!        DE VARIABLES INTERNES
!        DE SOUS-POINTS
!
! ----------------------------------------------------------------------
!
    integer :: nbcou, npgh, nbsect, nbfibr, nbvari, jcompo, jdcel, jnbsp
    integer :: itab(2), iret
    logical :: lteatt
!
! ----------------------------------------------------------------------
!
    call jevech('PDCEL_I', 'E', jdcel)
!
!     PCOMPOR CONTIENT 1 SEULE CMP : NBVARI
!     LES AUTRES INFO VIENNENT DE PNBSP_I
!     SI PCOMPOR N'EST PAS FOURNI : NCMP_DYN = 0
!     ------------------------------------------------
    call tecach('ONN', 'PCOMPOR', 'L', 2, itab,&
                iret)
    if (itab(1) .ne. 0) then
        call assert(itab(2).eq.1)
        jcompo=itab(1)
        read (zk16(jcompo-1+1),'(I16)') nbvari
    else
        nbvari=0
    endif
!
! --- PAR DEFAUT POUR TOUS LES ELEMENTS
    zi(jdcel-1+1) = 1
    zi(jdcel-1+2) = nbvari
!
!     PNBSP_I : INFOS NECESSSAIRES AU CALCUL DU NOMBRE DE SOUS-POINTS.
!     SI LE CHAMP N'EST PAS DONNE ==> VALEUR PAR DEFAUT
    call tecach('NNN', 'PNBSP_I', 'L', 1, jnbsp,&
                iret)
    if (jnbsp .eq. 0) goto 9999
!
! --- CAS DES ELEMENTS "COQUE EPAISSE" (MULTI-COUCHE) :
    if ((nomte.eq.'MEC3QU9H') .or. (nomte.eq.'MEC3TR7H') .or. (nomte.eq.'METCSE3') .or.&
        (nomte.eq.'METDSE3') .or. (nomte.eq.'MECXSE3')) then
        nbcou = zi(jnbsp-1+1)
        npgh = 3
        zi(jdcel-1+1) = npgh*nbcou
!
! --- CAS DES ELEMENTS "DKT"
        else if ( (nomte.eq.'MEDKQU4') .or. (nomte.eq.'MEDKTR3') .or.&
    (nomte.eq.'MEDSQU4') .or. (nomte.eq.'MEDSTR3') .or. (&
    nomte.eq.'MEQ4QU4') .or. (nomte.eq.'MET3TR3')) then
        nbcou = zi(jnbsp-1+1)
        npgh = 3
        zi(jdcel-1+1) = npgh*nbcou
!
! --- CAS DES ELEMENTS  "TUYAU" :
        else if ((nomte.eq.'MET3SEG3') .or. (nomte.eq.'MET6SEG3') .or.&
    (nomte.eq.'MET3SEG4')) then
        nbcou = zi(jnbsp-1+1)
        nbsect = zi(jnbsp-1+2)
        zi(jdcel-1+1) = (2*nbsect+1)* (2*nbcou+1)
!
! --- CAS DES ELEMENTS DE POUTRE "MULTIFIBRES" :
    else if (nomte.eq.'MECA_POU_D_EM') then
        nbfibr = zi(jnbsp-1+1)
        zi(jdcel-1+1) = nbfibr
!
    else if (nomte.eq.'MECA_POU_D_TGM') then
        nbfibr = zi(jnbsp-1+1)
        zi(jdcel-1+1) = nbfibr
    endif
!
9999  continue
end subroutine

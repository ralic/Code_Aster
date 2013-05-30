subroutine cmqlql(main, maout, nbma, lima)
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
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/cmqlma.h'
    include 'asterfort/cmqlnm.h'
    include 'asterfort/cmqlno.h'
    include 'asterfort/cpclma.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nbma, lima(nbma)
    character(len=8) :: main, maout
! ----------------------------------------------------------------------
!         TRANSFORMATION DES MAILLES QUADRATIQUES -> LINEAIRE
!-----------------------------------------------------------------------
!               SEG3            --> SEG2
!               TRIA6           --> TRIA3,
!               QUAD8,QUAD9     --> QUAD4,
!               TETRA10         --> TETRA4
!               PYRAM13         --> PYRMA5
!               PENTA15,PENTA18 --> PENTA6
!               HEXA20,HEXA27   --> HEWA8
! ----------------------------------------------------------------------
! IN        MAIN   K8  NOM DU MAILLAGE INITIAL
! IN/JXOUT  MAOUT  K8  NOM DU MAILLAGE TRANSFORME
! IN        NBMA    I  NOMBRE DE MAILLES A TRAITER
! IN        LIMA    I  NUMERO DES MAILLES A TRAITER
! ----------------------------------------------------------------------
!
!
    integer :: nbtyma
    parameter (nbtyma=27)
    integer :: i, jmq, icount, ityp, jnm, jdim, iret, tymaq(nbtyma), nbnm, jtyp
    character(len=8) :: nom, nomast(nbtyma)
    character(len=24) :: dimeq, nom1, nom2
    character(len=32) :: typma
!
!     TYMAQ: TYPE DES MAILLES QUADRATIQUES (CF. CATALOGUE TYPE_MAILLE__)
!     NOMAST: NOM DES TYPES DE MAILLES     (CF. CATALOGUE TYPE_MAILLE__)
    data tymaq   /3*0,4,4*0,9,4*0,14,0,16,2*0,19,0,21,22,0,24,0,26,27/
    data nomast / 'POI1    ', 'SEG2    ', 'SEG22   ', 'SEG3    ',&
     &              'SEG33   ', 'SEG4    ', 'TRIA3   ', 'TRIA33  ',&
     &              'TRIA6   ', 'TRIA66  ', 'TRIA7   ', 'QUAD4   ',&
     &              'QUAD44  ', 'QUAD8   ', 'QUAD88  ', 'QUAD9   ',&
     &              'QUAD99  ', 'TETRA4  ', 'TETRA10 ', 'PENTA6  ',&
     &              'PENTA15 ', 'PENTA18 ', 'PYRAM5  ', 'PYRAM13 ',&
     &              'HEXA8   ', 'HEXA20  ', 'HEXA27  '/
!     ------------------------------------------------------------------
    call jemarq()
!
!     =========================================================
!     VERIFICATION QUE LE CATALOGUE DES TYPES DE MAILLE N'A PAS
!     ETE MODIFIE
!     =========================================================
!
    do 10 ityp = 1, nbtyma
        call jenuno(jexnum('&CATA.TM.NOMTM', ityp), nom)
! VERIFICATION COHERENCE CATALOGUE FORTRAN
        call assert(nomast(ityp) .eq. nom)
10  end do
!
!     =========================================================
!     - ON VERIFIE SI DES MAILLES FOURNIES SONT QUADRATIQUES
!     - ON FILTRE EN NE RECUPERANT QUE LES MAILLES : SEG3,TRIA6,
!       QUAD8,QUAD9,TETRA10,PYRAM13,PENTA15,HEXA20,HEXA27.
!     =========================================================
!     ON RECUPERE LES NUMEROS DES MAILLES QUADRATIQUES:
    nom1 = '&&CMQLQL.MAIL_QUAD'
    call wkvect(nom1, 'V V I', nbma, jmq)
    typma=main//'.TYPMAIL'
    call jeveuo(typma, 'L', jtyp)
    icount=0
    do 20 i = 1, nbma
        if (tymaq(zi(jtyp+lima(i)-1)) .ne. 0) then
            icount=icount+1
            zi(jmq+icount-1)=lima(i)
        endif
20  end do
!
!     NOMBRE DE MAILLES QUADRATIQUES :
    nbma = icount
    if (nbma .eq. 0) call u2mess('F', 'MODELISA3_8')
!
!     ===============================
!     RECUPERATION DES NOEUDS MILIEUX
!     ===============================
    nom2 = '&&CMQLQL.NOEUD_MILIEU'
    call cmqlnm(main, nom1, nbma, nom2, nbnm)
    call jeveuo(nom2, 'L', jnm)
!
!     =========================================================
!     DUPLICATION DE CERTAINS OBJETS DE LA SD MAILLAGE INITIALE
!     AVANT REACTUALISATION
!     =========================================================
    call jedupo(main//'.NOMMAI', 'G', maout//'.NOMMAI', .false.)
    call jedupo(main//'.DIME', 'G', maout//'.DIME', .false.)
    call jedupo(main//'.TYPMAIL', 'G', maout//'.TYPMAIL', .false.)
!
    call jeexin(main//'.GROUPEMA', iret)
    if (iret .ne. 0) then
        call cpclma(main, maout, 'GROUPEMA', 'G')
    endif
!
!     ACTUALISATION DE '.DIME'
    dimeq=maout//'.DIME'
    call jeveuo(dimeq, 'E', jdim)
    zi(jdim)=zi(jdim)-nbnm
!
!     ==================================================================
!     MISE A JOUR DES NOEUDS ('.COORDO','.NOMNOE','.GROUPENO')
!     ==================================================================
!
    call cmqlno(main, maout, nbnm, zi(jnm))
!
!     =============================================
!     MISE A JOUR DES MAILLES ('.CONNEX','.TYPMAI')
!     =============================================
!
    call cmqlma(main, maout, nbma, zi(jmq))
!
    call jedetr('&&CMQLQL.MAIL_QUAD')
    call jedetr('&&CMQLQL.NOEUD_MILIEU')
!
    call jedema()
!
end subroutine

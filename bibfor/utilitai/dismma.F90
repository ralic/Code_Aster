subroutine dismma(questi, nomobz, repi, repkz, ierd)
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
    implicit none
!     --     DISMOI(MAILLAGE)
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/ltnotb.h'
    include 'asterfort/tbliva.h'
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=32) :: repk
    character(len=8) :: nomob
    character(len=*) :: nomobz, repkz
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOB  : NOM D'UN OBJET DE TYPE MAILLAGE
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPK   : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    complex(kind=8) :: c16b
    character(len=19) :: table
    character(len=1) :: k1bid
    real(kind=8) :: zmax, zmin
    integer :: jdime, ibid, ier, ilmaco, ism, k, nbma, nbno
    integer :: nbsm, nno, typv, jtypma
    character(len=8) :: kbid, typma
!
!
!
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
    call jeveuo(nomob//'.DIME', 'L', jdime)
!
!
    if (questi .eq. 'NB_MA_MAILLA') then
!     ---------------------------------
        repi = zi(jdime-1+3)
!
!
    else if (questi.eq.'NB_SM_MAILLA') then
!     ---------------------------------
        repi = zi(jdime-1+4)
!
!
    else if (questi.eq.'NB_NO_MAILLA') then
!     ---------------------------------
        repi = zi(jdime-1+1)
!
!
    else if (questi.eq.'NB_NL_MAILLA') then
!     ---------------------------------
        repi = zi(jdime-1+2)
!
!
    else if (questi.eq.'NB_NO_SS_MAX') then
!     ---------------------------------
        nbsm = zi(jdime-1+4)
        repi = 0
        do 10,ism = 1,nbsm
        call jelira(jexnum(nomob//'.SUPMAIL', ism), 'LONMAX', nno, kbid)
        repi = max(repi,nno)
10      continue
!
!
    else if (questi.eq.'Z_CST') then
!     ---------------------------------
        call ltnotb(nomob, 'CARA_GEOM', table)
        call tbliva(table, 0, ' ', ibid, 0.d0,&
                    c16b, k1bid, 'ABSO', 0.d0, 'Z_MIN',&
                    k1bid, ibid, zmin, c16b, k1bid,&
                    ier)
        call tbliva(table, 0, ' ', ibid, 0.d0,&
                    c16b, k1bid, 'ABSO', 0.d0, 'Z_MAX',&
                    k1bid, ibid, zmax, c16b, k1bid,&
                    ier)
!
        if (zmin .eq. zmax) then
            repk = 'OUI'
        else
            repk = 'NON'
        endif
!
!
    else if (questi.eq.'Z_ZERO'.or.questi.eq.'DIM_GEOM') then
!     ------------------------------------------------------------
        call ltnotb(nomob, 'CARA_GEOM', table)
        call tbliva(table, 0, ' ', ibid, 0.d0,&
                    c16b, k1bid, 'ABSO', 0.d0, 'Z_MIN',&
                    k1bid, ibid, zmin, c16b, k1bid,&
                    ier)
        call tbliva(table, 0, ' ', ibid, 0.d0,&
                    c16b, k1bid, 'ABSO', 0.d0, 'Z_MAX',&
                    k1bid, ibid, zmax, c16b, k1bid,&
                    ier)
!
        if (zmin .eq. zmax .and. zmin .eq. 0.d0) then
            repk = 'OUI'
        else
            repk = 'NON'
        endif
!
        if (questi .eq. 'DIM_GEOM') then
!     ----------------------------------------
            repi = zi(jdime-1+6)
!          -- ON RETOURNE 2 SI Z=0. PARTOUT :
            if ((repi.eq.3) .and. (repk.eq.'OUI')) then
                repi=2
            endif
            repk='???'
        endif
!
!
    else if (questi.eq.'DIM_GEOM_B') then
!     ----------------------------------------
        repi = zi(jdime-1+6)
        repk='???'
!
!
    else if (questi.eq.'NB_NO_MA_MAX') then
!     ----------------------------------------
        nbma = zi(jdime-1+3)
        call jeveuo(jexatr(nomob//'.CONNEX', 'LONCUM'), 'L', ilmaco)
        repi = 0
        do 40,k = 1,nbma
        nbno = zi(ilmaco+k) - zi(ilmaco-1+k)
        repi = max(repi,nbno)
40      continue
!
!
        else if ((questi.eq.'EXI_TRIA3' ) .or. (questi.eq.'EXI_TRIA6'&
    ) .or. (questi.eq.'EXI_QUAD4' ) .or. (questi.eq.'EXI_QUAD8'&
    ) .or. (questi.eq.'EXI_QUAD9' ) .or. (questi.eq.'EXI_SEG2'&
    ) .or. (questi.eq.'EXI_SEG3' ) .or. (questi.eq.'EXI_HEXA8'&
    ) .or. (questi.eq.'EXI_HEXA20' ) .or. (questi.eq.'EXI_HEXA27' )&
    .or. (questi.eq.'EXI_PENTA6' ) .or. (questi.eq.'EXI_PENTA15')&
    .or. (questi.eq.'EXI_TETRA4' ) .or. (questi.eq.'EXI_TETRA10')&
    .or. (questi.eq.'EXI_PYRAM5' ) .or. (questi.eq.'EXI_PYRAM13')&
    .or. (questi.eq.'EXI_POI1' )) then
!     ----------------------------------------
        typma='XXXX'
        if (questi .eq. 'EXI_TRIA3') typma='TRIA3'
        if (questi .eq. 'EXI_TRIA6') typma='TRIA6'
        if (questi .eq. 'EXI_QUAD4') typma='QUAD4'
        if (questi .eq. 'EXI_QUAD8') typma='QUAD8'
        if (questi .eq. 'EXI_QUAD9') typma='QUAD9'
        if (questi .eq. 'EXI_SEG2') typma='SEG2'
        if (questi .eq. 'EXI_SEG3') typma='SEG3'
        if (questi .eq. 'EXI_HEXA8') typma='HEXA8'
        if (questi .eq. 'EXI_HEXA20') typma='HEXA20'
        if (questi .eq. 'EXI_HEXA27') typma='HEXA27'
        if (questi .eq. 'EXI_PENTA6') typma='PENTA6'
        if (questi .eq. 'EXI_PENTA15') typma='PENTA15'
        if (questi .eq. 'EXI_TETRA4') typma='TETRA4'
        if (questi .eq. 'EXI_TETRA10') typma='TETRA10'
        if (questi .eq. 'EXI_PYRAM5') typma='PYRAM5'
        if (questi .eq. 'EXI_PYRAM13') typma='PYRAM13'
        if (questi .eq. 'EXI_POI1') typma='POI1'
        call assert(typma.ne.'XXXX')
        call jenonu(jexnom('&CATA.TM.NOMTM', typma), typv)
        call assert(typv.gt.0)
!
        repk = 'NON'
        nbma = zi(jdime-1+3)
        call jeveuo(nomob//'.TYPMAIL', 'L', jtypma)
        do 50,k = 1,nbma
        if (zi(jtypma-1+k) .eq. typv) goto 51
50      continue
        goto 52
51      continue
        repk='OUI'
52      continue
!
    else
        ierd = 1
    endif
!
!
!
    repkz = repk
    call jedema()
end subroutine

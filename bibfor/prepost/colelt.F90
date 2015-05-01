subroutine colelt(nbnode, maxnod, nbtyma, nbmail, nbnoma,&
                  nuconn)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.greffet at edf.fr
    implicit none
!
!      COLELT --   LECTURE DES NUMEROS DES ELEMENTS, DE LEUR TYPE,
!                  DE LEUR NUMERO DE GROUPE, DU NOMBRE DE LEURS
!                  CONNECTIVITES ET DE LEURS CONNECTIVITES
!
!   ARGUMENT        E/S  TYPE         ROLE
!    MAXNOD         IN    I         NOMBRE MAXIMUM DE NOEUDS POUR
!                                   UNE MAILLE DONNEE
!    NBTYMA         IN    I         NOMBRE  DE TYPES DE MAILLES
!    NBMAIL         OUT   I         NOMBRE TOTAL DE MAILLES
!    NBNOMA         IN    I         NOMBRE DE NOEUDS DE LA MAILLE
!                                    POUR UN TYPE DE MAILLE DONNEE
!    NUCONN         IN    I         PASSAGE DE LA NUMEROTATION DES NDS
!                                     D'UNE MAILLE : ASTER -> GMSH
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    integer :: maxnod, nbtyma, nbmail, nbnoma(nbtyma), nuconn(15, 32), nbnode
! -----  VARIABLES LOCALES
    character(len=8) :: k8bid
    aster_logical :: exisgr
!
!
    integer :: jnuma, jtypma, jnbnma, jnoma, jnbmag, jnbtym
    integer :: jindma, ij, icurgr, nbgrou, indgro, ima
    integer :: ityp, ino, node, i, indmax, jgr
    integer, pointer :: noeuds(:) => null()
    integer, pointer :: mailles(:) => null()
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
    call jemarq()
!
! --- INITIALISATION :
!     --------------
    k8bid = '        '
!
! --- LECTURE DU NOMBRE D'ELEMENTS :
!     ----------------------------
    nbmail = nbnode
!
! --- CREATION DE VECTEURS DE TRAVAIL :
!     -------------------------------
    call jedetr('&&PRECOU.NUMERO.MAILLES')
    call jedetr('&&PRECOU.TYPE.MAILLES')
!FH    CALL JEDETR('&&PRECOU.GROUPE.MAILLES')
    call jedetr('&&PRECOU.NBNO.MAILLES')
    call jedetr('&&PRECOU.CONNEC.MAILLES')
    call jedetr('&&PRECOU.NBMA.GROUP_MA')
    call jedetr('&&PRECOU.NBTYP.MAILLES')
    call jedetr('&&PRECOU.LISTE.GROUP_MA')
    call jedetr('&&PRECOU.INDICE.GROUP_MA')
!
! --- VECTEUR DES NUMEROS DES MAILLES
    call wkvect('&&PRECOU.NUMERO.MAILLES', 'V V I', nbmail, jnuma)
! --- VECTEUR DU TYPE DES MAILLES
    call wkvect('&&PRECOU.TYPE.MAILLES', 'V V I', nbmail, jtypma)
! --- VECTEUR DU NUMERO DE GROUPE DES MAILLES
!FH      CALL WKVECT('&&PRECOU.GROUPE.MAILLES','V V I',NBMAIL,JGROMA)
    call jeveuo('&&PRECOU.GROUPE.MAILLES', 'L', vi=mailles)
! --- VECTEUR DU NOMBRE DE CONNECTIVITES DES MAILLES
    call wkvect('&&PRECOU.NBNO.MAILLES', 'V V I', nbmail, jnbnma)
! --- VECTEUR DES CONNECTIVITES DES MAILLES
    call wkvect('&&PRECOU.CONNEC.MAILLES', 'V V I', maxnod*nbmail, jnoma)
! --- VECTEUR DU NOMBRE DE MAILLES POUR UN GROUPE DE MAILLES
    call wkvect('&&PRECOU.NBMA.GROUP_MA', 'V V I', nbmail, jnbmag)
! --- VECTEUR DU NOMBRE DE MAILLES PAR TYPE DE MAILLES
    call wkvect('&&PRECOU.NBTYP.MAILLES', 'V V I', nbtyma, jnbtym)
! --- CREATION DU VECTEUR FAISANT CORRESPONDRE LES INDICES AUX
! --- NUMEROS DES GROUPES DE MAILLES :
    call wkvect('&&PRECOU.INDICE.GROUP_MA', 'V V I', nbmail, jindma)
! --- INDICATION DE DESTRUCTION DES NOEUDS
    call jeveuo('&&PRECOU.DETR.NOEUDS', 'E', vi=noeuds)
!
!
! --- LECTURE DES ENREGISTREMENTS RELATIFS AUX MAILLES ET AFFECTATION
! --- DES VECTEURS DE TRAVAIL :
!     -----------------------
    ij = 0
!
! --- ICURGR : NUMERO DU GROUPE COUPLE
!     NBGROU : NBRE DE GROUPES TROUVES
!     INDGRO : INDICE DU GROUPE
    icurgr = 0
    nbgrou = 0
    indgro = 0
    do 10 ima = 1, nbmail
!
! NUMERO DE LA MAILLE
        zi(jnuma - 1 + ima) = ima
! TYPE DE LA MAILLE (TOUJOURS UN POINT)
        zi(jtypma - 1 + ima) = 15
! NOMBRE DE NOEUDS PAR MAILLE (TOUJOURS UN POINT)
        zi(jnbnma - 1 + ima) = 1
! NUMERO DES NOEUDS DE LA MAILLE (TOUJOURS UN POINT)
        zi(jnoma - 1 + ij+1) = ima
!
!      INDICATION DES NOEUDS QUI NE SONT PAS ORPHELINS
        ityp = zi(jtypma+ima-1)
        do 12 ino = 1, nbnoma(ityp)
            node = zi(jnoma+ij+nuconn(ityp,ino)-1)
            noeuds(node+1) = 1
 12     continue
!
        if (icurgr .ne. mailles(ima)) then
            icurgr = mailles(ima)
            exisgr = .false.
            do 20 i = 1, nbgrou
! CAS_VIV ICURGR=0 et ZI(JINDMA+I-1)=1
                if (icurgr .eq. zi(jindma+i-1)) then
                    exisgr = .true.
                    indgro = i
                    goto 30
                endif
 20         continue
 30         continue
            if (.not.exisgr) then
                nbgrou = nbgrou + 1
                indgro = nbgrou
                zi(jindma+indgro-1) = mailles(ima)
            endif
        endif
!MH
        indgro = 1
!MH
        zi(jnbmag+indgro-1) = zi(jnbmag+indgro-1) + 1
!
        ij = ij + zi(jnbnma+ima-1)
        zi(jnbtym+zi(jtypma+ima-1)-1) = zi(jnbtym+zi(jtypma+ima-1)-1)+ 1
 10 end do
!
    indmax = nbgrou
!MH
!      INDMAX = 1
!MH
    call jeecra('&&PRECOU.INDICE.GROUP_MA', 'LONUTI', indmax)
!
! --- CREATION DE LA COLLECTION DES GROUPES DE MAILLES :
!     ------------------------------------------------
    call jecrec('&&PRECOU.LISTE.GROUP_MA', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                indmax)
    call jeecra('&&PRECOU.LISTE.GROUP_MA', 'LONT', nbmail)
!
    do 40 i = 1, indmax
        call jeecra(jexnum('&&PRECOU.LISTE.GROUP_MA', i), 'LONMAX', zi( jnbmag+i-1))
        zi(jnbmag+i-1) = 0
 40 end do
!
! --- AFFECTATION DES OBJETS RELATIFS AUX GROUPES DE MAILLES :
!     ------------------------------------------------------
! --- ICURGR : NUMERO DU GROUPE COUPLE
!     NBGROU : NBRE DE GROUPES TROUVES
!     INDGRO : INDICE DU GROUPE
    icurgr = 0
    nbgrou = 0
    indgro = 0
    do 50 ima = 1, nbmail
        if (icurgr .ne. mailles(ima)) then
            icurgr = mailles(ima)
            exisgr = .false.
            do 60 i = 1, nbgrou
                if (icurgr .eq. zi(jindma+i-1)) then
                    exisgr = .true.
                    indgro = i
                    goto 70
                endif
 60         continue
 70         continue
            if (.not.exisgr) then
                nbgrou = nbgrou + 1
                indgro = nbgrou
            endif
        endif
!MH
!   INDGRO = 1
!MH
        zi(jnbmag+indgro-1) = zi(jnbmag+indgro-1) + 1
!
        zi(jindma+indgro-1) = mailles(ima)
        call jeveuo(jexnum('&&PRECOU.LISTE.GROUP_MA', indgro), 'E', jgr)
        zi(jgr+zi(jnbmag+indgro-1)-1) = zi(jnuma+ima-1)
 50 end do
!
    call jedema()
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine

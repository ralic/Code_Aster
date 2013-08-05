subroutine xprpls(dnomo, dcnsln, dcnslt, nomo, noma,&
                  cnsln, cnslt, grln, grlt, corres,&
                  ndim, ndomp, edomg)
    implicit none
!
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescns.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnsprj.h"
#include "asterfort/detrsd.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pj2dco.h"
#include "asterfort/pj3dco.h"
    character(len=8) :: dnomo, nomo, noma
    character(len=16) :: corres
    character(len=19) :: dcnsln, dcnslt, cnsln, cnslt, grln, grlt, ndomp, edomg
    integer :: ndim
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     ------------------------------------------------------------------
!
!       XPRPLS   : X-FEM PROPAGATION : PROJECTION DES LEVEL SETS
!       ------     -     --            -              -     -
!
!  DANS LE CADRE DE LA PROPAGATION X-FEM AVEC LA METHODE UPWIND, ON PEUT
!  UTILISER DEUX MAILLAGES DIFFERENTS POUR LE MODELE PHYSIQUE ET POUR LA
!  REPRESENTATION DES LEVEL SETS. DANS CE CAS, ON DOIT PROJECTER
!  LES LEVEL SETS SUR LE MAILLAGE PHYSIQUE A PARTIR DU MAILLAGE UTILISE
!  POUR LA REPRESENTATION DES LEVEL SETS.
!
!  ENTREE
!  ------
!
!    * MODELE POUR LA REPRESENTATION DES LEVEL SETS
!      --------------------------------------------
!     DNOMO  = NOM DU MODELE
!     DCNSLN = CHAMP_NO_S DES VALEURS DE LA LEVEL SET NORMALE
!     DCNSLT = CHAMP_NO_S DES VALEURS DE LA LEVEL SET TANGENTE
!     EDOMG  = VOIR XPRDOM.F
!
!
!    * MODELE PHYSIQUE
!      ---------------
!     NOMO   = NOM DU MODELE
!     NOMA   = NOME DU MAILLAGE
!     CNSLN  = CHAMP_NO_S DES VALEURS DE LA LEVEL SET NORMALE
!     CNSLT  = CHAMP_NO_S DES VALEURS DE LA LEVEL SET TANGENTE
!     GRLN   = CHAMP_NO_S DES VALEURS DU GRADIENT DE CNSLN
!     GRLT   = CHAMP_NO_S DES VALEURS DU GRADIENT DE CNSLT
!     NDOMP  = VOIR XPRDOM.F
!
!     NDIM   = DIMENSION DU MODELE (DNOMO OU NOMO)
!
!
!  SORTIE
!  ------
!     CORRES = NOM DU OBJET JEVEUX OU ON PEUT STOCKER LA CORRESPONDANCE
!              ENTRE LES DEUX MAILLAGES
!     CNSLN  = CHAMP_NO_S DES NOUVELLES VALEURS DE LA LEVEL SET NORMALE
!              POUR LE MODELE PHYSIQUE
!     CNSLT  = CHAMP_NO_S DES NOUVELLES VALEURS DE LA LEVEL SET TANGENTE
!              POUR LE MODELE PHYSIQUE
!     GRLN   = CHAMP_NO_S DES NOUVELLES VALEURS DU GRADIENT DE CNSLN
!              POUR LE MODELE PHYSIQUE
!     GRLT   = CHAMP_NO_S DES NOUVELLES VALEURS DU GRADIENT DE CNSLT
!              POUR LE MODELE PHYSIQUE
!
!     ------------------------------------------------------------------
!
!
!     CHARACTERISTICS OF THE MESHES
!
!     PROJECTION LEVEL SETS MESH
    integer :: nbelpr, jefrom, jcnlnv, jcnltv, jcnlnl, jcnltl
!
!     PROJECTION PHYSICAL MESH
    character(len=19) :: tmplsn, tmplst
    integer :: jnto, nunopr, jtmplt, jtmpln
!
!     PROJECTION CODE
    logical :: ldmax
    real(kind=8) :: distma
    character(len=8) :: lpain(4), lpaout(2)
    character(len=19) :: cnols, celgls, chams
    character(len=24) :: lchin(4), lchout(2), ligrel
!
!     GENERAL PURPOSE
    integer :: i, ibid
    character(len=8) :: k8b
    integer :: ifm, niv
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
    if (niv .ge. 0) then
        write(ifm,*)
        write(ifm,*)'   PROJECTION DES LEVEL SETS SUR LE MAILLAGE'//&
     &                  ' PHYSIQUE'
    endif
!
!     RETREIVE THE LIST OF THE NODES IN THE DOMAIN ON THE PHYSICAL MESH
    call jelira(ndomp, 'LONMAX', nunopr, k8b)
    call jeveuo(ndomp, 'L', jnto)
!
!     RETREIVE THE LIST OF THE ELEMENTS IN THE DOMAIN ON THE AUXILIARY
!     GRID
    call jelira(edomg, 'LONMAX', nbelpr, k8b)
    call jeveuo(edomg, 'L', jefrom)
!
!        PROJECT THE LEVELSETS FROM THE AUXILIARY MESH TO THE PHYSICAL
!        MESH. THE NODAL FIELD IS EXTRAPOLATED OUTSIDE THE AUXILIARY
!        MESH.
    ldmax = .false.
    distma = r8maem()
!
!        CREATE THE "CONNECTION" TABLE BETWEEN THE PHYSICAL AND
!        AUXILIARY MESHES
    if (ndim .eq. 2) then
        call pj2dco('PARTIE', dnomo, nomo, nbelpr, zi(jefrom),&
                    nunopr, zi(jnto), ' ', ' ', corres,&
                    ldmax, distma)
    else
        call pj3dco('PARTIE', dnomo, nomo, nbelpr, zi(jefrom),&
                    nunopr, zi(jnto), ' ', ' ', corres,&
                    ldmax, distma)
    endif
!
!        CREATE TWO TEMPORARY FIELDS WHERE THE PROJECTED VALUES WILL BE
!        STORED
    tmplsn = '&&OP0010.TMPLSN'
    tmplst = '&&OP0010.TMPLST'
!
!        PROJECTION OF THE NORMAL LEVELSET. THE EXISTING FIELD DATA
!        STRUCTURES ARE AUTOMATICALLY DESTROYED BY THE SUBROUTINE
!        "CNSPRJ"
    call cnsprj(dcnsln, corres, 'G', tmplsn, ibid)
    ASSERT(ibid.eq.0)
!
!        PROJECTION OF THE TANGENTIAL LEVELSET. THE EXISTING FIELD DATA
!        STRUCTURES ARE AUTOMATICALLY DESTROYED BY THE SUBROUTINE
!        "CNSPRJ"
    call cnsprj(dcnslt, corres, 'G', tmplst, ibid)
    ASSERT(ibid.eq.0)
!
    call jedetr(corres)
!
!        RETREIVE THE EXISTING NORMAL LEVEL SET FIELD
    call jeveuo(cnsln//'.CNSV', 'E', jcnlnv)
    call jeveuo(cnsln//'.CNSL', 'E', jcnlnl)
!
!        RETREIVE THE EXISTING TANGENTIAL LEVEL SET FIELD
    call jeveuo(cnslt//'.CNSV', 'E', jcnltv)
    call jeveuo(cnslt//'.CNSL', 'E', jcnltl)
!
!        RETREIVE THE TEMPORARY FIELDS WHERE THE PROJECTED VALUES OF THE
!        LEVEL SET HAVE BEEN STORED
    call jeveuo(tmplsn//'.CNSV', 'L', jtmpln)
    call jeveuo(tmplst//'.CNSV', 'L', jtmplt)
!
!        SUBSTITUTE THE PROJECTED VALUES OF THE LEVEL SETS INTO THE
!        EXISTING LEVEL SET FIELDS ONLY FOR THE NODES IN THE PHYSICAL
!        MESH INVOLVED IN THE PROJECTION
    do 3000 i = 1, nunopr
!
!           SUBSTITUTE THE PROJECTED VALUES OF THE NORMAL LEVEL SET INTO
!           THE EXISTING NORMAL LEVEL SET OF THE PHYSICAL MESH
        zr(jcnlnv-1+zi(jnto-1+i)) = zr(jtmpln-1+zi(jnto-1+i))
        zl(jcnlnl-1+zi(jnto-1+i)) = .true.
!
!           SUBSTITUTE THE PROJECTED VALUES OF THE TANGENTIAL LEVEL SET
!           INTO THE EXISTING TANGENTIAL LEVEL SET OF THE PHYSICAL MESH
        zr(jcnltv-1+zi(jnto-1+i)) = zr(jtmplt-1+zi(jnto-1+i))
        zl(jcnltl-1+zi(jnto-1+i)) = .true.
!
3000  continue
!
!        DESTROY THE TEMPORARY PROJECTED LEVEL SETS
    call detrsd('CHAM_NO_S', tmplsn)
    call detrsd('CHAM_NO_S', tmplst)
!
! ----------------------------------------------------------------------
!        CALCULATE THE GRADIENTS OF THE LEVEL SETS OF THE PHYSICAL MESH
! ----------------------------------------------------------------------
!
    if (niv .ge. 0) then
        write(ifm,*)'   CALCUL DES GRADIENTS DES LEVEL SETS SUR'//&
     &                  ' LE MAILLAGE PHYSIQUE'
    endif
!
!        NORMAL LEVEL SET
    ligrel = nomo//'.MODELE'
    cnols = '&&OP0010.GR.CNOLS'
    celgls = '&&OP0010.GR.CELGLS'
    chams = '&&OP0010.GR.CHAMS'
!
    call cnscno(cnsln, ' ', 'NON', 'V', cnols,&
                'F', ibid)
    lpain(1)='PGEOMER'
    lchin(1)=noma//'.COORDO'
    lpain(2)='PNEUTER'
    lchin(2)=cnols
    lpaout(1)='PGNEUTR'
    celgls = '&&OP0010.GR.CELGLS'
    lchout(1)=celgls
!
    call calcul('S', 'GRAD_NEUT_R', ligrel, 2, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
    call celces(celgls, 'V', chams)
    call cescns(chams, ' ', 'V', grln, ' ',&
                ibid)
!
    call detrsd('CHAM_NO', cnols)
    call detrsd('CHAM_ELEM', celgls)
    call detrsd('CHAM_ELEM_S', chams)
!
!        TANGENTIAL LEVEL SET
    call cnscno(cnslt, ' ', 'NON', 'V', cnols,&
                'F', ibid)
    lpain(1)='PGEOMER'
    lchin(1)=noma//'.COORDO'
    lpain(2)='PNEUTER'
    lchin(2)=cnols
    lpaout(1)='PGNEUTR'
    celgls = '&&OP0010.GR.CELGLS'
    lchout(1)=celgls
!
    call calcul('S', 'GRAD_NEUT_R', ligrel, 2, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
    call celces(celgls, 'V', chams)
    call cescns(chams, ' ', 'V', grlt, ' ',&
                ibid)
!
    call detrsd('CHAM_NO', cnols)
    call detrsd('CHAM_ELEM', celgls)
    call detrsd('CHAM_ELEM_S', chams)
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine

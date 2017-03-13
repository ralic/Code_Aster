subroutine irecri(nomcon, form, ifi, titre, lgmsh,&
                  nbcham, cham, partie, nbpara, para,&
                  nbordr, ordr, lresu, motfac, iocc,&
                  cecr, tycha, lcor, nbnot, numnoe,&
                  nbmat, nummai, nbcmp, nomcmp, lsup,&
                  borsup, linf, borinf, lmax, lmin,&
                  formr, versio,niv)
! aslint: disable=W1504
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/irch19.h"
#include "asterfort/irgmsh.h"
#include "asterfort/irpara.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerecu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutrg.h"
#include "asterfort/titre2.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nomcon
    character(len=*) :: form, titre, cham(*), para(*)
    character(len=*) :: motfac, cecr
    character(len=*) :: nomcmp(*), formr, partie
    character(len=8) :: tycha
    real(kind=8) :: borsup, borinf
    integer :: versio, nbcham, nbpara, niv
    integer :: nbordr, ordr(*), nbcmp, iocc
    integer :: nbnot, numnoe(*), nbmat, nummai(*)
    aster_logical :: lresu, lcor
    aster_logical :: lsup, linf, lmax, lmin, lgmsh
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!     ECRITURE D'UN CONCEPT SUR FICHIER RESULTAT
!
! IN  NOMCON : K8  : NOM DU CONCEPT A IMPRIMER
! IN  FORM   : K8  : FORMAT D'ECRITURE
! IN  IFI    : IS  : UNITE LOGIQUE D'ECRITURE
! IN  TITRE  : K80 : TITRE POUR ALI_BABA ET SUPERTAB
! IN  NBCHAM : I   : NOMBRE DE CHAMP DANS LE TABLEAU CHAM
! IN  CHAM   : K16 : NOM DES CHAMPS A IMPRIMER ( EX 'DEPL', ....
! IN  PARTIE : K4  : IMPRESSION DE LA PARTIE COMPLEXE OU REELLE DU CHAMP
! IN  NBPARA : I   : NOMBRE DE PARAMETRES LE TABLEAU PARA
! IN  PARA   : K16 : NOM DES PARAMETRES A IMPRIMER ( EX 'OMEGA2', ...
! IN  NBORDR : I   : NOMBRE DE NUMEROS D'ORDRE DANS LE TABLEAU ORDR
! IN  ORDR   : I   : LISTE DES NUMEROS D'ORDRE A IMPRIMER
! IN  LRESU  : L   : INDIQUE SI NOMCON EST UN CHAMP OU UN RESULTAT
! IN  MOTFAC : K   : NOM DU MOT CLE FACTEUR
! IN  IOCC   : I   : NUMERO D'OCCURENCE DU MOT CLE FACTEUR
! IN  MODELE : K   : NOM DU MODELE
! IN  CECR   : K1  : CODE D'ECRITURE DES PARAMETRES
!                    'T' TABLEAU 'L' LISTE
! IN  LCOR   : L   : INDIQUE SI IMPRESSION DES COORDONNEES DES NOEUDS
!                    .TRUE.  IMPRESSION
! IN  TYCHA  : K8  : TYPE DE CHAMP (SCALAIRE,VECT_2D,VECT_3D,TENS_2D,
!                    TENS_3D) POUR LE FORMAT GMSH (VERSION >= 1.2)
! IN  NBNOT  : I   : NOMBRE DE NOEUDS A IMPRIMER
! IN  NUMNOE : I   : NUMEROS DES NOEUDS A IMPRIMER
! IN  NBMAT  : I   : NOMBRE DE MAILLES A IMPRIMER
! IN  NUMMAI : I   : NUMEROS DES MAILLES A IMPRIMER
! IN  NBCMP  : I   : NOMBRE DE COMPOSANTES A IMPRIMER
! IN  NOMCMP : K8  : NOMS DES COMPOSANTES A IMPRIMER
! IN  LSUP   : L   : =.TRUE. INDIQUE PRESENCE D'UNE BORNE SUPERIEURE
! IN  BORSUP : R   : VALEUR DE LA BORNE SUPERIEURE
! IN  LINF   : L   : =.TRUE. INDIQUE PRESENCE D'UNE BORNE INFERIEURE
! IN  BORINF : R   : VALEUR DE LA BORNE INFERIEURE
! IN  LMAX   : L   : =.TRUE. INDIQUE IMPRESSION VALEUR MAXIMALE
! IN  LMIN   : L   : =.TRUE. INDIQUE IMPRESSION VALEUR MINIMALE
! IN  FORMR  : K   : FORMAT D'ECRITURE DES REELS SUR "RESULTAT"
! IN  VERSIO : I   : NIVEAU VERSION GMSH 1 OU 2
! IN  NIV    : I   : NIVEAU IMPRESSION MOT CLE INFO
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    character(len=6) :: chnumo
    character(len=8) :: nomco
    character(len=19) :: noch19
    character(len=24) :: nomst
    aster_logical :: lordr
    integer :: nbrk16, ibid
    integer :: i, ifi, isy
    integer :: iordr
    integer :: iret
    integer :: jtitr
    integer :: nbtitr
!     ------------------------------------------------------------------
!     --- IMPRESSION D'UN TABLEAU SYNTHETIQUE DES PARAMETRES-----
!         (UNIQUEMENT FORMAT 'RESULTAT')
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    if (niv .gt. 1) then
      call irpara(nomcon, form, ifi, nbordr, ordr,nbpara, para, cecr)
    endif  
!
    nomst = '&&IRECRI.SOUS_TITRE.TITR'
    nomco = nomcon
!
!
!     --------------------------
!     TRAITEMENT DU FORMAT GMSH
!     -------------------------
!
    if (form .eq. 'GMSH') then
!
        call irgmsh(nomcon, partie, ifi, nbcham, cham,&
                    lresu, nbordr, ordr, nbcmp, nomcmp,&
                    nbmat, nummai, versio, lgmsh, tycha)
!
!     -----------------------------
!     TRAITEMENT DES AUTRES FORMATS
!     -----------------------------
!
    else
!
!     *******************************************
!     --- BOUCLE SUR LA LISTE DES NUMEROS D'ORDRE
!     *******************************************
        nbrk16 = 0
!
        do iordr = 1, nbordr
            call jemarq()
            call jerecu('V')
!
!       --- SI VARIABLE DE TYPE RESULTAT = RESULTAT COMPOSE :
!           VERIFICATION CORRESPONDANCE ENTRE NUMERO D'ORDRE
!           UTILISATEUR ORDR(IORDR) ET NUMERO DE RANGEMENT IRET
! AU CAS OU ON NE PASSE PAS EN DESSOUS ON INITIALISE LORDR A FALSE
            lordr=.false.
            if (lresu) then
                call rsutrg(nomcon, ordr(iordr), iret, ibid)
                if (iret .eq. 0) then
!           - MESSAGE NUMERO D'ORDRE NON LICITE
                    call codent(ordr(iordr), 'G', chnumo)
                    call utmess('A', 'PREPOST2_46', sk=chnumo)
                    goto 22
                endif
                lordr=.true.
            endif
!
!       --- BOUCLE SUR LE NOMBRE DE CHAMPS A IMPRIMER
            if (nbcham .ne. 0) then
                do isy = 1, nbcham
                    if (lresu) then
!           * RESULTAT COMPOSE
!             - VERIFICATION EXISTENCE DANS LA SD RESULTAT NOMCON
!               DU CHAMP CHAM(ISY) POUR LE NO. D'ORDRE ORDR(IORDR)
!               ET RECUPERATION DANS NOCH19 DU NOM SE LE CHAM_GD EXISTE
                        call rsexch(' ', nomcon, cham(isy), ordr(iordr), noch19,&
                                    iret)
                        if (iret .ne. 0) goto 20
                    else
!           * CHAM_GD
                        noch19 = nomcon
                    endif
!
!           * IMPRESSION DES PARAMETRES (FORMAT 'RESULTAT')
                    if (lordr .and. form .eq. 'RESULTAT') then
!             - SEPARATION DES DIVERS NUMEROS D'ORDRE PUIS IMPRESSION
                        write(ifi,'(/,1X,A)') '======>'
                        call irpara(nomcon, form, ifi, 1, ordr(iordr),&
                                    nbpara, para, cecr)
                        lordr=.false.
                    endif
!           * CREATION D'UN SOUS-TITRE
                    if (form .eq. 'RESULTAT' .or. form .eq. 'IDEAS') then
                        if (lresu) then
                            call titre2(nomcon, noch19, nomst, motfac, iocc,&
                                        formr, cham(isy), ordr(iordr))
                        else
                            call titre2(nomcon, noch19, nomst, motfac, iocc,&
                                        formr)
                        endif
                    endif
!
!           * IMPRESSION DU SOUS-TITRE SI FORMAT 'RESULTAT'
                    if (form .eq. 'RESULTAT') then
!              ---- SEPARATION DES DIVERS CHAMPS -----
                        write(ifi,'(/,1X,A)') '------>'
                        call jeveuo(nomst, 'L', jtitr)
                        call jelira(nomst, 'LONMAX', nbtitr)
                        write(ifi,'(1X,A)') (zk80(jtitr+i-1),i=1,&
                        nbtitr)
                    endif
!
!           ********************************************************
!           * IMPRESSION DU CHAMP (CHAM_NO OU CHAM_ELEM) AU FORMAT
!             'RESULTAT' OU 'SUPERTAB'
!                LE CHAMP EST UN CHAM_GD SIMPLE SI LRESU=.FALSE. OU
!                LE CHAMP EST LE CHAM_GD CHAM(ISY) DE NUMERO D'ORDRE
!                ORDR(IORDR) ISSU DE LA SD_RESULTAT NOMCON
                    call irch19(noch19, form, ifi, titre,&
                                nomcon, cham(isy), ordr(iordr), lcor, nbnot,&
                                numnoe, nbmat, nummai, nbcmp, nomcmp,&
                                lsup, borsup, linf, borinf, lmax,&
                                lmin, lresu, formr)
 20                 continue
                end do
            endif
 22         continue
            call jedema()
        end do
!
    endif
!
!     --- DESTRUCTION OBJETS DE TRAVAIL
    call jedetr('&&IRECRI.CHPRES')
    call jedetr('&&IRECRI.FVIDAV')
    call jedetr('&&IRECRI.FVIDAP')
    call jedetr('&&IRECRI.NOM_ACC')
    call jedetr('&&IRECRI.TABLE.TOT')
    call jeexin(nomst, iret)
    if (iret .ne. 0) call jedetr(nomst)
!
    call jedema()
end subroutine

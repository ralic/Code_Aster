subroutine nbptca(ligrel, option, param, obnbpt, obnbno)
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
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/alchml.h'
    include 'asterfort/celces.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: ligrel, option, param, obnbpt, obnbno
! ------------------------------------------------------------------
! BUT: CREER L'OBJET OBNBPT QUI CONTIENDRA LE NOMBRE DE POINTS
!      DE DISCRETISATION (POUR LES MAILLES D'UN LIGREL)
!      POUR LE CHAMP ASSOCIE A UN PARAMETRE D'UNE OPTION
! ------------------------------------------------------------------
!     ARGUMENTS:
! LIGREL  IN/JXIN  K19 : LIGREL
! OPTION  IN       K16 : NOM D'UNE OPTION DE CALCUL
! PARAM   IN       K8  : NOM D'UN PARAMETRE DE OPTION
! OBNBPT  IN/JXOUT K24 : OBJET QUI CONTIENDRA LES NOMBRES DE POINTS
! OBNBNO  IN/JXOUT K24 : OBJET QUI CONTIENDRA LES NOMBRES DE NOEUDS
! ------------------------------------------------------------------
! REMARQUES :
!  CETTE ROUTINE PEUT ETRE UTILISEE PAR EXEMPLE POUR DETERMINER LES
!  NOMBRE DE POINTS DE GAUSS D'UN MODELE MECANIQUE NON-LINEAIRE:
!  OPTION = 'RAPH_MECA' + PARAM='PCONTMR'
!
!  L'OBJET CREE EST UN VECTEUR D'ENTIERS DIMENSIONNE AU NOMBRE DE
!  MAILLES DU MAILLAGE : V(IMA) : NBPT(MAILLE_IMA)
!  LES MAILLES TARDIVES SONT IGNOREES.
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: ibid, iret, nbma, ima, jcesd, jnbpt, jnbno, iacnx1, ilcnx1, nbno
    character(len=1) :: kbid
    character(len=8) :: ma
    character(len=19) :: cel, ces
!     ------------------------------------------------------------------
    call jemarq()
    cel = '&&NBPTCA.CEL'
    ces = '&&NBPTCA.CES'
!
    call dismoi('F', 'NOM_MAILLA', ligrel, 'LIGREL', ibid,&
                ma, ibid)
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                kbid, ibid)
    call wkvect(obnbpt, 'V V I', nbma, jnbpt)
    call wkvect(obnbno, 'V V I', nbma, jnbno)
    call jeveuo(ma//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
!
    call alchml(ligrel, option, param, 'V', cel,&
                iret, ' ')
    if (iret .ne. 0) then
!       - IL N'Y A RIEN A FAIRE : NBPT(IMA)=0
    else
        call celces(cel, 'V', ces)
        call jeveuo(ces//'.CESD', 'L', jcesd)
        do 10,ima = 1,nbma
        zi(jnbpt-1+ima) = zi(jcesd-1+5+4* (ima-1)+1)
        nbno = zi(ilcnx1+ima) - zi(ilcnx1-1+ima)
        zi(jnbno-1+ima) = nbno
10      continue
!
    endif
!
!
    call detrsd('CHAM_ELEM', cel)
    call detrsd('CHAM_ELEM_S', ces)
!
    call jedema()
end subroutine

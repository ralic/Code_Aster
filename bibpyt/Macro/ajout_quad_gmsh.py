#@ MODIF ajout_quad_gmsh Macro  DATE 14/09/2004   AUTEUR MCOURTOI M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
# (AT YOUR OPTION) ANY LATER VERSION.                                                  
#                                                                       
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
#                                                                       
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.        
# ======================================================================



# script PYTHON de modification d'un fichier de maillage GMSH (*.msh)
# pour transformer les mailles lineiques en mailles quadratiques
# SEG2->SEG3 / TRIA3->TRIA6 / QUAD4->QUAD8,
# TETRA4 -> TETRA10 / PENTA6 -> PENTA15 / PYRAM5 -> PYRAM13

def ajout_quad_gmsh(texte):

  import os,sys,copy

  try:
# construction du dictionnaire nom du noeud / coordonnees : dict_no
# construction de la liste des noeuds : l_no

    typ_mail={}
    typ_mail['LI']=['1']
    typ_mail['2D']=['2','3']
    typ_mail['3D']=['4','5','6','7']
    typ_mail['PO']=['15']

    texte_eclate=texte.split('\n')
    nbno=int(texte_eclate[texte_eclate.index('$NOD')+1])
    l_no=texte_eclate[texte_eclate.index('$NOD')+2:texte_eclate.index('$ENDNOD')]
    dict_no={}
    for i in texte_eclate[texte_eclate.index('$NOD')+2:texte_eclate.index('$ENDNOD')]:
      cc=i.split(' ')
      dict_no[cc[0]]=[]
      for j in cc[1:]:dict_no[cc[0]].append(float(j))

    l_el1=texte_eclate[texte_eclate.index('$ELM')+2:texte_eclate.index('$ENDELM')]
    nbel =texte_eclate[texte_eclate.index('$ELM')+1]


# construction du dictionnaire : element / liste des aretes de l'element : elems_aretes
# et de son inverse            : aretes / elements contenant cette arete : aretes_elems

    aretes_elems={}
    elems_aretes={}
    l_el=[]
    for elem in l_el1 :
        connec0=elem.split(' ')[5:]
        while '' in connec0 : connec0.remove('')
        aa=elem.split(' ')[:4]

# attention : indicateur de type de maille : ajouter 7 pour passer
# de TRIA3 a TRIA6, de SEG2 a SEG3, de QUAD4 a QUAD8 (voir inigms.f)

#   si maille POI1, on ne fait rien

        if aa[1] not in typ_mail['PO'] : typel=str(int(aa[1])+7)
        else                           : typel=aa[1]
        nom_elem=aa[0]+' '+typel+' '+aa[2]+' '+aa[3]
        segments=[]

# traitement des mailles lineaires : un seul segment
        if   aa[1] in typ_mail['LI']:
           segments.append([int(connec0[0]),int(connec0[1])])

# traitement des mailles planes (TRI3 QUA4) : on cree les segments en prenant les noeuds consecutivement, puis on ferme
        elif aa[1] in typ_mail['2D']:
           for i in range(len(connec0)-1) : segments.append([int(connec0[i]),int(connec0[i+1])])
           segments.append([int(connec0[-1]),int(connec0[0])])

# traitement des mailles TETRA4 : pour comprendre, voir "fonctions de forme des elements isoparametriques" R3.01.01
        elif aa[1]=='4':
           for i in range(0,2) : segments.append([int(connec0[i]),int(connec0[i+1])])
           segments.append([int(connec0[2]),int(connec0[0])])
           for i in range(0,3) : segments.append([int(connec0[3]),int(connec0[i])])

# traitement des mailles HEXA8 : pour comprendre, voir "fonctions de forme des elements isoparametriques" R3.01.01
        elif aa[1]=='5':
           for i in range(0,3) : segments.append([int(connec0[i]),int(connec0[i+1])])
           segments.append([int(connec0[3]),int(connec0[0])])
           for i in range(4,7) : segments.append([int(connec0[i]),int(connec0[i+1])])
           segments.append([int(connec0[7]),int(connec0[4])])
           for i in range(0,4) : segments.append([int(connec0[i]),int(connec0[i+4])])

# traitement des mailles PENTA6 : pour comprendre, voir "fonctions de forme des elements isoparametriques" R3.01.01
        elif aa[1]=='6':
           for i in range(0,2) : segments.append([int(connec0[i]),int(connec0[i+1])])
           segments.append([int(connec0[2]),int(connec0[0])])
           for i in range(3,5) : segments.append([int(connec0[i]),int(connec0[i+1])])
           segments.append([int(connec0[5]),int(connec0[3])])
           for i in range(0,3) : segments.append([int(connec0[i]),int(connec0[i+3])])

# traitement des mailles PYRA5 : pour comprendre, voir "fonctions de forme des elements isoparametriques" R3.01.01
        elif aa[1]=='7':
           for i in range(0,3) : segments.append([int(connec0[i]),int(connec0[i+1])])
           segments.append([int(connec0[3]),int(connec0[0])])
           for i in range(0,4) : segments.append([int(connec0[4]),int(connec0[i])])

        elems_aretes[nom_elem]=[]
        for seg in segments :
            elems_aretes[nom_elem].append(copy.copy(seg))
            seg.sort()
        for seg in segments :
           if aretes_elems.has_key(tuple(seg)):aretes_elems[tuple(seg)].append(nom_elem)
           else                               :aretes_elems[tuple(seg)]=[nom_elem]
        l_el.append(nom_elem)

# construction de la liste complete des aretes

    l_ar=aretes_elems.keys()
    l_ar.sort()

# numero du plus grand noeud :
    nmax=int(l_no[-1].split(' ')[0])

# nouveau nombre total de noeuds :
    ndtot=nbno+len(l_ar)

# insertion des noeuds milieux dans le dictionnaire "aretes" et la liste "l_no"  :
    ind=nmax
    for i in l_ar:
      ind=ind+1
      x=(dict_no[str(i[0])][0]+dict_no[str(i[1])][0])/2
      y=(dict_no[str(i[0])][1]+dict_no[str(i[1])][1])/2
      z=(dict_no[str(i[0])][2]+dict_no[str(i[1])][2])/2
      l_no.append(str(ind)+' '+str(x)+' '+str(y)+' '+str(z))
      for elem in aretes_elems[i]:
        for ar in elems_aretes[elem]:
            k=copy.copy(ar)
            k.sort()
            kk=tuple(k)
            if i==kk:
              ar.insert(1,ind)

# re-ecriture du fichier avec les noeuds milieux :

    resu='$NOD\n'+str(ndtot)+'\n'
    for i in l_no:
      resu=resu+i+'\n'
    resu=resu+'$ENDNOD\n'+'$ELM\n'+nbel+'\n'
    for i in l_el:
      aa=i.split(' ')[:4]
      if aa[1] not in typ_mail['PO']:
         typ=str(int(aa[1])-7)
      else : typ=aa[1]
      if elems_aretes[i]!=[]:
        resu=resu+i

# traitement des mailles lineaires : d'abord les noeuds sommets, puis le noeud milieu
        if typ in typ_mail['LI']:
          resu=resu+' 3 '
          for j in elems_aretes[i]:
              resu=resu+str(j[0])+' '+str(j[2])+' '+str(j[1])+' '

# traitement des mailles planes (TRI3 QUA4) : d'abord les noeuds sommets, puis, dans le meme ordre, les noeuds milieux
        elif typ in typ_mail['2D']:
          resu=resu+' '+str(len(elems_aretes[i])*2)+' '
          for j in elems_aretes[i]:
              resu=resu+str(j[0])+' '
          for j in elems_aretes[i]:
              resu=resu+str(j[1])+' '

# traitement des mailles TETRA4 : pour comprendre, voir "fonctions de forme des elements isoparametriques" R3.01.01
        elif typ=='4':
          resu=resu+' 10 '
          for j in elems_aretes[i][:4]:
              resu=resu+str(j[0])+' '
          resu=resu+str(elems_aretes[i][0][1])+' '
          resu=resu+str(elems_aretes[i][1][1])+' '
          resu=resu+str(elems_aretes[i][2][1])+' '
          resu=resu+str(elems_aretes[i][3][1])+' '
          resu=resu+str(elems_aretes[i][4][1])+' '
          resu=resu+str(elems_aretes[i][5][1])+' '

# traitement des mailles HEXA8 : pour comprendre, voir "fonctions de forme des elements isoparametriques" R3.01.01
        elif typ=='5':
          resu=resu+' 20 '
          for j in elems_aretes[i][:8]:
              resu=resu+str(j[0])+' '
          resu=resu+str(elems_aretes[i][0][1])+' '
          resu=resu+str(elems_aretes[i][1][1])+' '
          resu=resu+str(elems_aretes[i][2][1])+' '
          resu=resu+str(elems_aretes[i][3][1])+' '
  
          resu=resu+str(elems_aretes[i][8][1])+' '
          resu=resu+str(elems_aretes[i][9][1])+' '
          resu=resu+str(elems_aretes[i][10][1])+' '
          resu=resu+str(elems_aretes[i][11][1])+' '

          resu=resu+str(elems_aretes[i][4][1])+' '
          resu=resu+str(elems_aretes[i][5][1])+' '
          resu=resu+str(elems_aretes[i][6][1])+' '
          resu=resu+str(elems_aretes[i][7][1])+' '

# traitement des mailles PENTA6 : pour comprendre, voir "fonctions de forme des elements isoparametriques" R3.01.01
        elif typ=='6':
          resu=resu+' 15 '
          for j in elems_aretes[i][:6]:
              resu=resu+str(j[0])+' '
          resu=resu+str(elems_aretes[i][0][1])+' '
          resu=resu+str(elems_aretes[i][1][1])+' '
          resu=resu+str(elems_aretes[i][2][1])+' '

          resu=resu+str(elems_aretes[i][6][1])+' '
          resu=resu+str(elems_aretes[i][7][1])+' '
          resu=resu+str(elems_aretes[i][8][1])+' '

          resu=resu+str(elems_aretes[i][3][1])+' '
          resu=resu+str(elems_aretes[i][4][1])+' '
          resu=resu+str(elems_aretes[i][5][1])+' '

# traitement des mailles PYRA5 : pour comprendre, voir "fonctions de forme des elements isoparametriques" R3.01.01
        elif typ=='7':
          resu=resu+' 13 '
          for j in elems_aretes[i][:4]:
              resu=resu+str(j[0])+' '
          resu=resu+str(elems_aretes[i][0][1])+' '
          resu=resu+str(elems_aretes[i][1][1])+' '
          resu=resu+str(elems_aretes[i][2][1])+' '

          resu=resu+str(elems_aretes[i][6][1])+' '
          resu=resu+str(elems_aretes[i][7][1])+' '
          resu=resu+str(elems_aretes[i][8][1])+' '

          resu=resu+str(elems_aretes[i][3][1])+' '
          resu=resu+str(elems_aretes[i][4][1])+' '
          resu=resu+str(elems_aretes[i][5][1])+' '

      else:
# traitement des mailles POI1 : on recopie la connectivite telle quelle, sans ajouter de nouveau noeud
        resu=resu+l_el1[l_el.index(i)]

      resu=resu+'\n'

    resu=resu+'$ENDELM\n'
    return resu
  except :
    return 0
